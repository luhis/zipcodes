#r "./packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open FSharp.Data

type ZipCodes = HtmlProvider<"https://pe.usps.com/Archive/HTML/DMMArchive20050106/print/L002.htm">

let table = ZipCodes.Load("https://pe.usps.com/Archive/HTML/DMMArchive20050106/print/L002.htm").Tables.``L002 3-Digit ZIP Code Prefix Matrix``

let rec secondToLast =
    function
    | []       -> None
    | h :: [_] -> Some(h)
    | _ :: t   -> secondToLast t

let extractState (s: string) = s.Split ' ' |> Seq.toList |> secondToLast

let toState =
    extractState >>
    function
    | Some a -> a
    | None -> ""

let rows = table.Rows |> Seq.map (fun r -> r.``3‑Digit ZIP Code Prefix`` + "," + (toState r.``Column A 3‑Digit Destinations Label to``))

System.IO.File.WriteAllLines ("zips.csv", rows)
