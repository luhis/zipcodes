#r "./packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open FSharp.Data

type ZipCodes = HtmlProvider<"https://pe.usps.com/Archive/HTML/DMMArchive20050106/print/L002.htm">

let table = ZipCodes.Load("https://pe.usps.com/Archive/HTML/DMMArchive20050106/print/L002.htm").Tables.``L002 3-Digit ZIP Code Prefix Matrix``

let rec secondToLast =
    function
    | []       -> None
    | h :: [_] -> Some(h)
    | _ :: t   -> secondToLast t

let split (c: char) (s: string) = s.Split c

let extractState = split ' ' >> Seq.toList >> secondToLast

let toState =
    extractState >>
    function
    | Some a -> a
    | None -> ""

let rowToCsv (row: ZipCodes.L0023DigitZipCodePrefixMatrix.Row) =
    let prefix = row.``3‑Digit ZIP Code Prefix``.TrimEnd 'N'
    prefix + "," + (toState row.``Column A 3‑Digit Destinations Label to``)

let notEndsX (row: ZipCodes.L0023DigitZipCodePrefixMatrix.Row) = row.``3‑Digit ZIP Code Prefix``.EndsWith "X" |> not

let rows = table.Rows |> Seq.filter notEndsX |> Seq.map rowToCsv |> Seq.toList

let csv = "prefix,state"::rows

let fileName = "zips.csv";
System.IO.File.Delete fileName
System.IO.File.WriteAllLines (fileName, csv)
