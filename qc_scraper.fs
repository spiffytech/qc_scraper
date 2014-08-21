let (@@) fn x = fn x

open System
open System.Net
open System.IO
open HtmlAgilityPack

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

//let f = otherFetch "http://localhost"
let f = otherFetch "http://questionablecontent.net/archive.php"
match f with
| Some h ->
    h
    //|> printfn "%A"
    |> Seq.map (fun t -> t.InnerHtml)
    |> Seq.iter (fun h -> printfn "%s" h)
| None -> printfn "None"
