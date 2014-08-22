let (@@) fn x = fn x

open System
open System.Net
open System.IO
open HtmlAgilityPack

[<AutoOpen>]
module DomainTypes =
    type Comic = {id:int; title:string; body:string; link:string}
    type ComicLink = {text:string; link:string}

module QCFetcher =
    open System.Text.RegularExpressions

    let archiveURL = "http://questionablecontent.net/archive.php"

    let makeComicLink id =
        sprintf "http://questionablecontent.net/view.php?comic=%d" id

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
            let id = extractID comicLink.text

            match id with
            | None -> None
            | Some id ->
                let body = fetchBody id
                match body with
                | None -> None
                | Some body ->
                    let link = makeComicLink id
                    Some {Comic.id=id; title=comicLink.text; body=body; link=link}
        )
        (*
        |> Seq.map (fun comicLink -> comicLink.)
        |> Seq.map (fun h -> extractID h)
        *)
        |> Seq.choose id

module DB =
    open System.Data
    open Mono.Data.Sqlite

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

    let retrieveStored conn =
        let cmd = new SqliteCommand("select * from comics", conn)
        cmd

    let getComic conn id =
        let sql = "select * from comics where id=$id"
        let cmd = new SqliteCommand(sql, conn)
        cmd.Parameters.AddWithValue("$id", id) |> ignore
        let res = cmd.ExecuteReader()
        res

    let getComics conn =
        let sql = "select * from comics"
        let cmd = new SqliteCommand(sql, conn)
        let reader = cmd.ExecuteReader()
        reader

    let upsertComic conn comic =
        let sql = sprintf "insert or replace into comics (title, body, id) values ($title, $body, $id)"
        let cmd = new SqliteCommand(sql, conn)
        cmd.Parameters.AddWithValue("$title", comic.title) |> ignore
        cmd.Parameters.AddWithValue("$body", comic.body) |> ignore
        cmd.Parameters.AddWithValue("$id", comic.id) |> ignore
        cmd.ExecuteNonQuery()
        ()

module main =
    open QCFetcher

    let printComics conn =
        let comics = DB.getComics conn
        comics

    [<EntryPoint>]
    let main _ =
        let conn = DB.dbConnect "blah.db"

        //let f = fetchArchives "http://localhost"
        let comicLinks = fetchArchives "http://questionablecontent.net/archive.php"
        match comicLinks with
        | Some comicLinks ->
            extractComics comicLinks
            |> Seq.iter (fun comic -> DB.upsertComic conn comic)
            0
        | None -> 
            printfn "None"
            255
