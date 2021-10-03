namespace BadCore.Text

open System

[<Struct>]
type ParserState =
    private
        { Lines: string array
          Position: ParserPosition }

module private ParserState =
    let private lineSeparators = [| "\r\n"; "\n"; "\r"; "\r\n" |]

    let private (|EndOfFile|NewLine|Char|) { Lines = lines; Position = position } =
        if position.Line >= Array.length lines then
            EndOfFile
        elif position.Column
             >= String.length lines.[position.Line] then
            NewLine
        else
            Char

    let create str =
        if String.IsNullOrEmpty(str) then
            { Lines = [||]
              Position = ParserPosition.initial }
        else
            { Lines = str.Split(lineSeparators, StringSplitOptions.None)
              Position = ParserPosition.initial }

    let currentLine { Lines = lines; Position = position } = lines.[position.Line]

    let currentChar { Lines = lines; Position = position } = lines.[position.Line].[position.Column]

    let nextChar inputState =
        let incrementColumn inputState =
            { inputState with
                  Position = ParserPosition.incrementColumn inputState.Position }

        let incrementLine inputState =
            { inputState with
                  Position = ParserPosition.incrementLine inputState.Position }

        match inputState with
        | Char -> Some(currentChar inputState), incrementColumn inputState
        | NewLine -> Some('\n'), incrementLine inputState
        | EndOfFile -> None, inputState
