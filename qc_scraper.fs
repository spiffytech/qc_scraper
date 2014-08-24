let (@@) fn x = fn x

open System
open System.Net
open System.IO
open HtmlAgilityPack

[<AutoOpen>]
module GenericUtils =
    let bindOption switchFunction twoTrackInput =
        match twoTrackInput with
        | Some s -> switchFunction s
        | None -> None

let makeComicLink id =
    sprintf "http://questionablecontent.net/view.php?comic=%d" id

[<AutoOpen>]
module DomainTypes =
    type Comic = {id:int; title:string; body:string; link:string}

module QCFetcher =
    open System.Text.RegularExpressions

    let archiveURL = "http://questionablecontent.net/archive.php"

    (*
    let fetchURL url =
        let req = WebRequest.Create(Uri(url))
        let resp = req.GetResponse().GetResponseStream()
        IO.StreamReader(resp).ReadToEnd()
        *)

    let fetchArchives url =
        let web = new HtmlWeb()
        let page = web.Load(url)
        //printfn "%s" page.DocumentNode.OuterHtml
        let nodes = page.DocumentNode.SelectNodes(@"//div[@id=""archive""]/a")
        match nodes with
        | null -> None
        | _ ->
            nodes
            |> Seq.map (fun node ->
                let link =
                    node.Attributes
                    |> Seq.find (fun attr -> attr.Name = "href")
                    |> (fun attr -> attr.Value)
                node.InnerHtml
            )
            |> Some

    let extractID str =
        let pattern = @"^.*Comic (?<id>\d+):.*$"
        let match_ = Regex.Match(str, pattern)        
        match match_.Success with
        | false ->
            printfn "Could not extract comic ID: %s" str
            None
        | true ->
            match_.Groups.["id"].Value
            |> int
            |> Some

    let fetchBody id =
        let comicURL = makeComicLink id
        let web = new HtmlWeb()
        let page = web.Load(comicURL)
        //printfn "%s" page.DocumentNode.OuterHtml
        let nodes = page.DocumentNode.SelectNodes(@"//div[@id=""news""]")
        match nodes with
        | null -> None
        | _ -> Some nodes.[0].InnerHtml

module DB =
    open System.Data
    open Mono.Data.Sqlite

    let dbToComic (row:SqliteDataReader) =
        {
            Comic.id = unbox<int> row.["id"];
            title = unbox row.["title"];
            body = unbox row.["body"];
            link = makeComicLink @@ unbox<int> row.["id"]
        }

    let migrateDB conn =
        let cmd = new SqliteCommand("create table comics (id int primary key, title text, body text)", conn)
        cmd.ExecuteNonQuery()
            |> ignore
        ()

    let dbConnect filename =
        let dbExists = File.Exists filename

        let connStr = sprintf "Data Source =%s;" filename
        let conn = new SqliteConnection(connStr)
        conn.Open()

        match dbExists with
        | true -> ()
        | false -> migrateDB conn

        conn

    let getComics conn =
        let sql = "select * from comics"
        let cmd = new SqliteCommand(sql, conn)
        let reader = cmd.ExecuteReader()
        seq {
            while reader.Read() do
                yield dbToComic reader
        }

    let doesComicExist conn comicID =
        let sql = "select id from comics where id=$id"
        let cmd = new SqliteCommand(sql, conn)
        cmd.Parameters.AddWithValue("$id", comicID) |> ignore
        let reader = cmd.ExecuteReader()
        seq {
            while reader.Read() do
                yield reader.["id"]
        }
        |> Seq.length > 0

    let getNewComics conn comicIDs =
        let sql = "select id from comics"
        let cmd = new SqliteCommand(sql, conn)
        let reader = cmd.ExecuteReader()
        let storedIDs = seq {
            while reader.Read() do
                yield unbox<int> reader.["id"]
        }

        (Set.ofSeq comicIDs, Set.ofSeq storedIDs)
            ||> Set.difference
            |> Set.toSeq

    let upsertComic conn comic =
        let sql = sprintf "insert or replace into comics (title, body, id) values ($title, $body, $id)"
        let cmd = new SqliteCommand(sql, conn)
        cmd.Parameters.AddWithValue("$title", comic.title) |> ignore
        cmd.Parameters.AddWithValue("$body", comic.body) |> ignore
        cmd.Parameters.AddWithValue("$id", comic.id) |> ignore
        cmd.ExecuteNonQuery() |> ignore
        ()

module RSS =
    open System.ServiceModel.Syndication
    open System.Xml

    let ofComic comic =
        let item = new SyndicationItem()
        let comicURL = makeComicLink comic.id
        item.Id <- comicURL
        item.Links.Add @@ new SyndicationLink(new Uri(comicURL))
        item.Title <- TextSyndicationContent comic.title

        let imgLink = sprintf "<img src='http://questionablecontent.net/comics/%d.png' />" comic.id
        item.Content <- new TextSyndicationContent(imgLink + comic.body, TextSyndicationContentKind.Html)
        item

    let ofComics comics =
        let baseURL = "http://questionablecontent.spiffyte.ch"
        let feed = new SyndicationFeed()
        feed.Id <- "http://questionablecontent.spiffyte.ch"
        feed.Title <- TextSyndicationContent "Questionable Content (unofficial)"
        feed.Description <- TextSyndicationContent "Unofficial (and reliable!) Questionable Content RSS feed"
        feed.LastUpdatedTime <- new DateTimeOffset(DateTime.Now);
        feed.Generator <- "Script by spiffytech"

        feed.Links.Add @@ new SyndicationLink(new Uri(baseURL + "/feed.xml"));

        feed.Links.Add @@ new SyndicationLink(new Uri(baseURL));

        feed.Items <- Seq.map ofComic comics
        feed

    let stringOfFeed (filename:string) (feed:SyndicationFeed) =
        let xmlWriter = XmlWriter.Create filename
        feed.SaveAsRss20 xmlWriter
        xmlWriter.Close()

module main =
    open QCFetcher

    let printComics conn =
        let comics = DB.getComics conn
        comics

    [<EntryPoint>]
    let main _ =
        let outFile = "feed.xml"
        let conn = DB.dbConnect "blah.db"

        //let f = fetchArchives "http://localhost"
        let comicTitles = fetchArchives "http://questionablecontent.net/archive.php"
        match comicTitles with
        | Some comicTitles ->
            comicTitles
            |> Seq.map (fun comicTitle ->
                (extractID comicTitle, comicTitle)
            )
            |> Seq.choose (fun (comicID, comicTitle) ->
                match comicID with
                | Some comicID -> Some (comicID,comicTitle)
                | None -> None
            )
            |> (fun l ->
                printfn "Filtering %d comics" @@ Seq.length l
                l
            )
            |> Seq.filter (fun (comicID, comicTitle) -> not @@ DB.doesComicExist conn comicID)
            |> (fun l ->
                printfn "Retrieving %d new comics" @@ Seq.length l
                l
            )
            |> Seq.map (fun (comicID, comicTitle) ->
                fetchBody comicID
                |> bindOption (fun comicBody ->
                    let comicLink = makeComicLink comicID
                    Some {Comic.id=comicID; title=comicTitle; body=comicBody; link=comicLink}
                )
            )
            |> Seq.choose id
            |> Seq.iter (fun comic -> DB.upsertComic conn comic)

            DB.getComics conn
                |> RSS.ofComics
                |> RSS.stringOfFeed outFile
            0
        | None -> 
            printfn "None"
            255
