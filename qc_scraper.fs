let (@@) fn x = fn x

open System
open System.Net
open System.IO
open HtmlAgilityPack

module QCFetcher =
    let archiveURL = "http://questionablecontent.net/archive.php"

    let fetchURL url =
        let req = WebRequest.Create(Uri(url))
        let resp = req.GetResponse().GetResponseStream()
        IO.StreamReader(resp).ReadToEnd()

    let otherFetch url =
        let web = new HtmlWeb()
        let page = web.Load(url)
        //printfn "%s" page.DocumentNode.OuterHtml
        let nodes = page.DocumentNode.SelectNodes(@"//div[@id=""archive""]/a")
        match nodes with
        | null -> None
        | _ -> Some nodes


module DB =
    open System.Data
    open Mono.Data.Sqlite

    let migrateDB conn =
        let cmd = SqliteCommand("create table comics (id int, title text, body text)", conn)
        cmd.ExecuteNonQuery()
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

module main =
    open QCFetcher

    [<EntryPoint>]
    let main args =
        DB.dbConnect "blah.db"

        //let f = otherFetch "http://localhost"
        let f = otherFetch "http://questionablecontent.net/archive.php"
        match f with
        | Some h ->
            h
            //|> printfn "%A"
            |> Seq.map (fun t -> t.InnerHtml)
            |> Seq.iter (fun h -> printfn "%s" h)
            0
        | None -> 
            printfn "None"
            255
