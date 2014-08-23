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
    type ComicLink = {text:string; link:string}

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
                {ComicLink.text=node.InnerHtml; link=link}
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

    let extractComics comicLinks =
        comicLinks
        |> Seq.take 3
        |> Seq.map (fun comicLink ->
            extractID comicLink.text
            |> bindOption (fun id ->
                fetchBody id
                |> bindOption (fun body ->
                    let link = makeComicLink id
                    Some {Comic.id=id; title=comicLink.text; body=body; link=link}
                )
            )
        )
        (*
        |> Seq.map (fun comicLink -> comicLink.)
        |> Seq.map (fun h -> extractID h)
        *)
        |> Seq.choose id

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

    let rssOfComic comic =
        let item = new SyndicationItem()
        let comicURL = makeComicLink comic.id
        item.Id <- comicURL
        item.Links.Add @@ new SyndicationLink(new Uri(comicURL))
        item.Title <- TextSyndicationContent comic.title

        let imgLink = sprintf "<img src='http://questionablecontent.net/comics/%d.png' />" comic.id
        item.Content <- new TextSyndicationContent(imgLink + comic.body, TextSyndicationContentKind.Html)
        item

    let rssOfComics comics =
        let baseURL = "http://questionablecontent.spiffyte.ch"
        let feed = new SyndicationFeed()
        feed.Id <- "http://questionablecontent.spiffyte.ch"
        feed.Title <- TextSyndicationContent "Questionable Content (unofficial)"
        feed.Description <- TextSyndicationContent "Unofficial (and reliable!) Questionable Content RSS feed"
        feed.LastUpdatedTime <- new DateTimeOffset(DateTime.Now);
        feed.Generator <- "Script by spiffytech"

        feed.Links.Add @@ new SyndicationLink(new Uri(baseURL + "/feed.xml"));

        feed.Links.Add @@ new SyndicationLink(new Uri(baseURL));

        feed.Items <- Seq.map rssOfComic comics
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
        let comicLinks = fetchArchives "http://questionablecontent.net/archive.php"
        match comicLinks with
        | Some comicLinks ->
            extractComics comicLinks
            |> Seq.iter (fun comic -> DB.upsertComic conn comic)

            DB.getComics conn
                |> RSS.rssOfComics
                |> RSS.stringOfFeed outFile
            0
        | None -> 
            printfn "None"
            255
