namespace BadCore.Extensions

open System

module String =
    let ofChars (chars: char seq) : string = chars |> Seq.toArray |> String
    let trim (str: string) : string = str.Trim()
