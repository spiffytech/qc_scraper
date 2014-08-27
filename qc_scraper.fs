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

let makeImgLink comicID ext =
    sprintf "http://questionablecontent.net/comics/%d.%s" comicID ext

let chooseTuple tupleSeq fn =
    let chooser t =
        let v = fn t
        match v with
        | Some v -> Some t
        | None -> None

    Seq.choose chooser tupleSeq

[<AutoOpen>]
module DomainTypes =
    type Comic = {id:int; title:string; body:string; link:string; date:System.DateTime; imgtype:String}

module LoggerConfig =
    open NLog
    open NLog.Config
    open NLog.Targets

    let setLogLevel level =
        let config = new LoggingConfiguration()
        let consoleTarget = new ColoredConsoleTarget()
        config.AddTarget("console", consoleTarget)
        let rule = new LoggingRule("*", level, consoleTarget)
        config.LoggingRules.Add rule
        LogManager.Configuration <- config

module QCFetcher =
    open System.Text.RegularExpressions
    open NLog
    let logger = LogManager.GetLogger("QCFetcher")

    let archiveURL = "http://questionablecontent.net/archive.php"

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
            logger.Warn(sprintf "Could not extract comic ID: %s" str)
            None
        | true ->
            match_.Groups.["id"].Value
            |> int
            |> Some

    let fetchPostDate comicID ext =
        async {
            let imgLink = makeImgLink comicID ext
            let req = WebRequest.Create(Uri(imgLink))
            req.Method <- "HEAD"

            try
                let! resp = req.AsyncGetResponse()
                let resp' =
                    resp
                    :?> HttpWebResponse
                logger.Debug(sprintf "#%d post date = %s" comicID @@ resp'.LastModified.ToString())
                return Some resp'.LastModified
            with  // 404, etc.
                | ex ->
                    logger.Warn(sprintf "Exception fetching #%d" comicID)
                    return None
        }

    let fetchBody id =
        logger.Info(sprintf "Fetching body for #%d" id)
        let comicURL = makeComicLink id
        let req = WebRequest.Create(Uri(comicURL))
        let resp = req.GetResponse()
        let page = HtmlDocument()
        let html =
            use reader = new StreamReader(resp.GetResponseStream())
            reader.ReadToEnd()
        page.LoadHtml(html)
        let bodyNode = page.DocumentNode.SelectSingleNode(@"//div[@id=""news""]")
        let body =
            match bodyNode with
            | null ->
                logger.Warn(sprintf "Could not retrieve body for#%d" id)
                None
            | _ -> Some bodyNode.InnerHtml

        let imgNode = page.DocumentNode.SelectSingleNode(@"//img[@id=""strip""]")
        let ext =
            match imgNode with
            | null ->
                logger.Warn(sprintf "Could not retrieve image link for#%d" id)
                None
            | _ ->
                imgNode.Attributes
                |> Seq.find (fun attr -> attr.Name = "src")
                |> (fun attr -> attr.Value)
                |> (fun str -> str.Substring(str.Length - 3))
                |> Some

        match (body, ext) with
        | (_, None) -> None
        | (None, _) -> None
        | (Some body, Some ext) -> Some (body, ext)

module DB =
    open System.Data
    open Mono.Data.Sqlite
    open NLog
    let logger = LogManager.GetLogger("QCFetcher")

    let dbToComic (row:SqliteDataReader) =
        {
            Comic.id = unbox<int> row.["id"];
            title = unbox row.["title"];
            body = unbox row.["body"];
            link = makeComicLink @@ unbox<int> row.["id"];
            //date = System.DateTime.Parse @@ unbox<string> row.["date"];
            date = System.DateTime.Now;
            //imgtype = unbox row.["imgtype"]
            imgtype = ""
        }

    let migrateDB conn =
        let cmd = new SqliteCommand("create table comics (id int primary key, title text, body text, date text, imgtype text)", conn)
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
        let sql = "select * from comics order by id desc"
        let cmd = new SqliteCommand(sql, conn)
        let reader = cmd.ExecuteReader()
        seq {
            while reader.Read() do
                yield dbToComic reader
        }
        |> Seq.cache

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
        logger.Debug(sprintf "Upserting comic %d" comic.id)
        let sql = sprintf "insert or replace into comics (title, body, date, id, imgtype) values ($title, $body, $date, $id, $imgtype)"
        let cmd = new SqliteCommand(sql, conn)
        cmd.Parameters.AddWithValue("$title", comic.title) |> ignore
        cmd.Parameters.AddWithValue("$body", comic.body) |> ignore
        cmd.Parameters.AddWithValue("$date", comic.date) |> ignore
        cmd.Parameters.AddWithValue("$id", comic.id) |> ignore
        cmd.Parameters.AddWithValue("$imgtype", comic.imgtype) |> ignore
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
        item.LastUpdatedTime <- DateTimeOffset comic.date

        let imgLink = sprintf "<img src='%s' />" @@ makeImgLink comic.id comic.imgtype
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

        feed.Items <- Seq.map ofComic comics
        feed

    let stringOfFeed (filename:string) (feed:SyndicationFeed) =
        let xmlWriter = XmlWriter.Create filename
        feed.SaveAsRss20 xmlWriter
        xmlWriter.Close()

module main =
    open QCFetcher
    open NLog
    LoggerConfig.setLogLevel LogLevel.Debug
    let logger = LogManager.GetLogger("main")

    let printComics conn =
        let comics = DB.getComics conn
        comics

    [<EntryPoint>]
    let main _ =
        let outFile = "feed.xml"
        let conn = DB.dbConnect "blah.db"

        (*
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
                logger.Debug(sprintf "Filtering %d comics" @@ Seq.length l)
                l
            )
            |> Seq.filter (fun (comicID, comicTitle) -> not @@ DB.doesComicExist conn comicID)
            |> (fun l ->
                logger.Debug(sprintf "Retrieving %d new comics" @@ Seq.length l)
                l
            )
            |> Seq.map (fun (comicID, comicTitle) ->
                fetchBody comicID
                |> bindOption (fun (comicBody,ext) ->
                    let postDate = fetchPostDate comicID ext
                    |> bindOption (fun postdate ->
                        Some {Comic.id=comicID; title=comicTitle; body=comicBody; link=comicLink; date=postDate, imgtype=ext}
                    )
                )
            )
            |> Seq.choose id
            |> Seq.iter (fun comic -> DB.upsertComic conn comic)

            DB.getComics conn
                |> Seq.take 15
                |> RSS.ofComics
                |> RSS.stringOfFeed outFile
                *)
        DB.getComics conn
            //|> Seq.filter (fun comic -> comic.id > 1710 && comic.id < 1725)
            |> (fun comics ->
                Seq.map (fun comic ->
                    fetchBody comic.id
                    //|> Async.RunSynchronously
                    |> bindOption (fun (body,ext) ->
                        fetchPostDate comic.id ext
                        |> Async.RunSynchronously
                        |> bindOption (fun postDate ->
                            Some (comic,body,ext,postDate)
                        )
                    )
                ) comics
                //|> Async.Parallel
                //|> Async.RunSynchronously
            )
            |> Seq.choose id
            |> Seq.map (fun (comic,body,ext,postDate) ->
                {comic with date=postDate}
            )
            |> Seq.iter (fun comic -> DB.upsertComic conn comic)
        0
            (*
        | None -> 
            logger.Error("Could not retrieve comic titles!")
            printfn "None"
            255
            *)
