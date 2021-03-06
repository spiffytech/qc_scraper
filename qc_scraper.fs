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

    let rec seqChunkedMap chunkSize fn s =
        seq {
            yield! fn @@ Seq.truncate chunkSize s
            match (Seq.length s) with
            | x when x <= chunkSize -> yield! Seq.empty
            | _ ->
                let skippedSeq = Seq.skip chunkSize s
                yield! seqChunkedMap chunkSize fn (Seq.skip chunkSize s)
        }

let makeComicLink id =
    sprintf "http://questionablecontent.net/view.php?comic=%d" id

let makeImgLink comicID ext =
    sprintf "http://questionablecontent.net/comics/%d.%s" comicID ext

let tupleChooser fn =
    (fun tuple ->
        let value = fn tuple
        match value with
        | Some _ -> Some tuple
        | None -> None
    )

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
        logger.Debug("Archives fetched")
        let nodes = page.DocumentNode.SelectNodes(@"//div[@id=""container""]/div[@class=""row""]/div[@class=""small-12 medium-expand column""]/a")
        match nodes with
        | null ->
            logger.Fatal("Could not parse archives")
            None
        | _ ->
            nodes
            |> Seq.map (fun node -> node.InnerHtml)
            |> Seq.cache
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

    let fetchPostDateAsync comicID ext =
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
                | _ ->
                    logger.Warn(sprintf "Exception fetching #%d" comicID)
                    return None
        }

    let fetchBodyExtAsync id =
        async {
            logger.Info(sprintf "Fetching body for #%d" id)
            let comicURL = makeComicLink id
            let req = WebRequest.Create(Uri(comicURL))
            let! resp = req.AsyncGetResponse()
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

            let ret =
                match (body, ext) with
                | (_, None) -> None
                | (None, _) -> None
                | (Some body, Some ext) -> Some (body, ext)
            return ret
        }

module DB =
    open System.Data
    open Mono.Data.Sqlite
    open NLog
    let logger = LogManager.GetLogger("DB")

    let dbToComic (row:SqliteDataReader) =
        {
            Comic.id = unbox<int> row.["id"];
            title = unbox row.["title"];
            body = unbox row.["body"];
            link = makeComicLink @@ unbox<int> row.["id"];
            date = System.DateTime.Parse @@ unbox<string> row.["date"];
            imgtype = unbox row.["imgtype"]
        }

    let migrateDB conn =
        let cmd = new SqliteCommand("create table comics (id int primary key, title text, body text, date text, imgtype text)", conn)
        cmd.ExecuteNonQuery()
            |> ignore
        ()

    let dbConnect filename =
        let dbExists = File.Exists filename

        let connStr = sprintf "Data Source =%s;Version=3;" filename
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
        let sql = "select count(id) from comics where id=$id"
        let cmd = new SqliteCommand(sql, conn)
        cmd.Parameters.AddWithValue("$id", comicID) |> ignore
        let numRows = cmd.ExecuteScalar()
        match Convert.ToInt32(numRows) with
        | x when x = 0 -> false
        | _ -> true

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
    open NLog
    let logger = LogManager.GetLogger("RSS")

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

    let ofComics feedURL comics =
        let feed = new SyndicationFeed()
        feed.Id <- "http://questionablecontent.spiffyte.ch"
        feed.Title <- TextSyndicationContent "Questionable Content (unofficial)"
        feed.Description <- TextSyndicationContent "Unofficial (but reliable!) Questionable Content RSS feed"
        feed.LastUpdatedTime <- new DateTimeOffset(DateTime.Now);
        feed.Generator <- "Script by spiffytech - https://github.com/spiffytech/qc_scraper"

        feed.Links.Add @@ new SyndicationLink(new Uri(feedURL))

        feed.Items <- Seq.map ofComic comics

        feed

    let stringOfFeed (filename:string) (feed:SyndicationFeed) =
        let xmlWriter = XmlWriter.Create filename
        feed.SaveAsAtom10 xmlWriter
        xmlWriter.Close()
        logger.Info("RSS file written")

module PubSubHubbub =
    open System.ServiceModel.Syndication
    open RestSharp
    open NLog
    let logger = LogManager.GetLogger("PubSubHubbub")

    //let addToFeed feedURL (feed:SyndicationFeed) =
    let addToFeed feedURL (feed:SyndicationFeed) =
        logger.Debug("Adding Pubsubhubbub support to the feed")

        let hubbubLink1 = new SyndicationLink(new Uri("https://pubsubhubbub.appspot.com/"))
        hubbubLink1.RelationshipType <- "hub"
        feed.Links.Add hubbubLink1

        let hubbubLink2 = new SyndicationLink(new Uri(feedURL))
        hubbubLink2.RelationshipType <- "self"
        feed.Links.Add hubbubLink2

        feed

    let notify feedURL =
        logger.Info("Notifying Pubsubhubbub")
        let client = RestClient "https://pubsubhubbub.appspot.com"
        let request = RestRequest("/", Method.POST)
        request.AddParameter("hub.mode", "publish") |> ignore
        request.AddParameter("hub.url", feedURL) |> ignore
        client.Execute request |> ignore

module main =
    open QCFetcher
    open PubSubHubbub
    open NLog
    LoggerConfig.setLogLevel LogLevel.Debug
    let logger = LogManager.GetLogger("main")

    let printComics conn =
        let comics = DB.getComics conn
        comics

    let updateComics comicIDs =
        comicIDs
        |> Seq.map (fun comicID ->
            async {
                let! properties = fetchBodyExtAsync comicID
                return
                    match properties with
                    | Some (body,ext) -> Some (comicID,body,ext)
                    | None ->
                        logger.Warn(sprintf "Could not fetch body/image extension for #%d" comicID)
                        None
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.choose id
        |> Seq.map (fun (comicID,body,ext) ->
            async {
                let! postDate = fetchPostDateAsync comicID ext
                let ret =
                    match postDate with
                    | Some postDate -> Some (comicID,body,ext,postDate)
                    | None ->
                        logger.Warn(sprintf "Could not fetch date for #%d" comicID)
                        None
                return ret
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.choose id

    [<EntryPoint>]
    let main args =
        let outfile =
            match args with
            | [|x|] -> x
            | [||]
            | _ -> "feed.xml"
        let conn = DB.dbConnect "blah.db"

        fetchArchives "http://questionablecontent.net/archive.php"
        |> bindOption (fun comicTitles ->
            let newComics =
                comicTitles
                //|> Seq.take 45
                |> Seq.map (fun comicTitle ->
                    let comicID = extractID comicTitle
                    // Using this instead of tupleChooser because it extracts the comicID value for us
                    match comicID with
                    | None ->
                        logger.Warn(sprintf "Could not extract ID from '%s'" comicTitle)
                        None
                    | Some comicID -> Some (comicID,comicTitle)
                )
                |> Seq.choose id
                //|> Seq.filter (fun (comicID,_) -> comicID > 1710 && comicID < 1725)
                |> Seq.filter (fun (comicID, comicTitle) -> not @@ DB.doesComicExist conn comicID)

            let updatedComics =
                newComics
                |> Seq.map (fun (id,_) -> id)
                |> seqChunkedMap 20 updateComics

            let hash = Map.ofSeq newComics
            updatedComics
            |> Seq.map (fun (comicID,body,ext,postDate) ->
                let title = hash.Item comicID
                {Comic.id=comicID; body=body; imgtype=ext; date=postDate; title=title; link=makeComicLink comicID}
            )
            |> Seq.iter (fun comic -> DB.upsertComic conn comic)

            let feedURL = "http://questionablecontent.spiffyte.ch/feed.xml"
            DB.getComics conn
                |> Seq.take 15
                |> (fun comics ->
                    let rangeStart =
                        comics
                        |> Seq.last
                        |> (fun comic -> comic.id)

                    let rangeFinish =
                        comics
                        |> Seq.head
                        |> (fun comic -> comic.id)
                    logger.Info(sprintf "Writing feed for #%d - #%d" rangeStart rangeFinish)
                    comics
                )
                |> RSS.ofComics feedURL
                |> PubSubHubbub.addToFeed feedURL
                |> RSS.stringOfFeed outfile

            notify feedURL

            conn.Close()

            Some "success"
        )
        |> (fun ret ->
            match ret with
            | None ->
                logger.Fatal("Could not retrieve comic titles!")
                exit 255
            | Some ret ->
                logger.Debug("Success! Exiting.")
                exit 0
        )
