namespace BadCore.Text

[<Struct>]
type ParserPosition = private { Line: int; Column: int }

module private ParserPosition =
    let initial = { Line = 0; Column = 0 }

    let incrementColumn position =
        { position with
              Column = position.Column + 1 }

    let incrementLine position =
        { Column = 0; Line = position.Line + 1 }
