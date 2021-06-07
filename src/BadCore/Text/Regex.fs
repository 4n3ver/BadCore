namespace BadCore.Text

module Regex =
    open System.Text.RegularExpressions

    let matches (pattern: string) (input: string) : string list seq =
        seq { for m in Regex.Matches(input, pattern) -> [ for g in m.Groups -> g.Value ] }
        |> Seq.map List.tail

    module Pattern =
        let (|RegexCaptures|_|) (pattern: string) (input: string) : string list option =
            let m = Regex.Match(input, pattern)

            if m.Success then
                [ for g in m.Groups -> g.Value ]
                |> List.tail
                |> Some
            else
                None
