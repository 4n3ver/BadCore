namespace BadCore.Text

type ErrorMessage = string
type ParserLabel = string

[<Struct>]
type ParserOk<'a> =
    private
        { Parsed: 'a
          State: ParserState }

[<Struct>]
type ParserError =
    private
        { Label: ParserLabel
          Message: ErrorMessage
          State: ParserState }

type ParserResult<'a> = Result<ParserOk<'a>, ParserError>

module ParserResult =
    let print result =
        match result with
        | Ok { Parsed = parsed } -> printfn $"%A{parsed}"
        | Error { State = state
                  Message = message
                  Label = label } ->
            let { Line = line; Column = col } = state.Position
            let errorLine = ParserState.currentLine state
            let failureCaret = sprintf "%*s^" col ""

            printfn
                $"Line: %i{line} Col: %i{col} Error parsing %s{label}\n\
                  %s{errorLine}\n\
                  %s{failureCaret}%s{message}"
