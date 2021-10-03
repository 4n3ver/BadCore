namespace BadCore.Text

open System
open BadCore
open BadCore.Extensions

[<Struct>]
type Parser<'a> =
    private
        { Parse: ParserState -> ParserResult<'a>
          Label: ParserLabel }

module Parser =
    [<AutoOpen>]
    module private Internal =
        let runOnInput { Parse = parse } = parse

        let runNOnInput n parser state =
            let rec parse n (prev: ParserOk<'a list>) =
                if (n > 0) then
                    match runOnInput parser prev.State with
                    | Ok curr ->
                        parse
                            (n - 1)
                            { Parsed = curr.Parsed :: prev.Parsed
                              State = curr.State }
                    | Error err -> Error err
                else
                    Ok(
                        { prev with
                              Parsed = List.rev prev.Parsed }
                    )

            parse n { Parsed = []; State = state }

        let runNOrMoreOnInput n parser state =
            let rec parse n (prev: ParserOk<'a list>) =
                match runOnInput parser prev.State with
                | Ok curr ->
                    parse
                        (n - 1)
                        { Parsed = curr.Parsed :: prev.Parsed
                          State = curr.State }
                | Error _ when n <= 0 ->
                    Ok(
                        { prev with
                              Parsed = List.rev prev.Parsed }
                    )
                | Error err -> Error err

            parse n { Parsed = []; State = state }

    let run (parser: Parser<'a>) (str: string) : ParserResult<'a> =
        str |> ParserState.create |> runOnInput parser

    let satisfy (predicate: char -> bool) (label: ParserLabel) : Parser<char> =
        let parser state =
            match ParserState.nextChar state with
            | Some char, state when predicate char -> Ok({ Parsed = char; State = state })
            | Some char, state ->
                Error(
                    { Message = $"Unexpected '%c{char}'!"
                      Label = label
                      State = state }
                )
            | None, state ->
                Error(
                    { Message = "No more input!"
                      Label = label
                      State = state }
                )

        { Parse = parser; Label = label }

    let andThen (parser1: Parser<'a>) (parser2: Parser<'b>) : Parser<'a * 'b> =
        let (>>=) = Result.op_GreaterGreaterEquals

        let parser state =
            runOnInput parser1 state
            >>= (fun { Parsed = parsed1; State = state } ->
                runOnInput parser2 state
                >>= (fun { Parsed = parsed2; State = state } ->
                    Ok(
                        { Parsed = parsed1, parsed2
                          State = state }
                    )))

        { Parse = parser
          Label = $"(%s{parser1.Label} andThen %s{parser2.Label})" }

    let orElse (parser1: Parser<'a>) (parser2: Parser<'a>) : Parser<'a> =
        let parser state =
            match runOnInput parser1 state with
            | Error _ -> runOnInput parser2 state
            | res -> res

        { Parse = parser
          Label = $"(%s{parser1.Label} orElse %s{parser2.Label})" }

    let getLabel (parser: Parser<'a>) : string = parser.Label

    let setLabel (parser: Parser<'a>) (label: ParserLabel) : Parser<'a> =
        let parser state =
            match runOnInput parser state with
            | Ok res -> Ok res
            | Error err -> Error({ err with Label = label })

        { Parse = parser; Label = label }

    let (<?>) = setLabel
    let (.>>.) = andThen
    let (<|>) = orElse


    // Monad
    let unit (x: 'a) : Parser<'a> =
        { Parse = (fun state -> Ok({ Parsed = x; State = state }))
          Label = $"(unit %A{x})" }

    let bind (f: 'a -> Parser<'b>) (m: Parser<'a>) : Parser<'b> =
        let parser state =
            match runOnInput m state with
            | Ok { Parsed = parsed; State = state } -> runOnInput (f parsed) state
            | Error err -> Error err

        { Parse = parser
          Label = $"(bind %s{m.Label})" }


    // Functor
    let map (f: 'a -> 'b) (m: Parser<'a>) : Parser<'b> =
        bind (f >> unit) m <?> $"(map %s{m.Label})"


    // Applicative
    let apply (fm: Parser<'a -> 'b>) (m: Parser<'a>) : Parser<'b> =
        fm |> andThen <| m |> map (fun (f, p) -> f p)
        <?> $"(apply %s{fm.Label} %s{m.Label})"


    let inline (<<|) f x = map f x
    let inline (|>>) x f = f <<| x
    let inline (.>>) p1 p2 = p1 .>>. p2 |>> fst
    let inline (>>.) p1 p2 = p1 .>>. p2 |>> snd
    let inline (>>%) p x = p |>> Function.constant x


    // Derived
    let (<!>) = map
    let (<*>) = apply
    let (=<<) = bind
    let inline (>>=) m f = f =<< m
    let inline (>=>) f1 f2 = bind f2 << f1
    let inline (<=<) f2 f1 = f1 >=> f2
    let lift = (<!>)
    let inline lift2 f m1 m2 = f <!> m1 <*> m2
    let inline lift3 f m1 m2 m3 = f <!> m1 <*> m2 <*> m3
    let inline (<*) x y = lift2 (fun l _ -> l) x y
    let inline ( *> ) x y = lift2 (Function.constant id) x y

    let traverseSeqA f m =
        let prepend x y = Seq.append (Seq.singleton x) y
        let foldBack = Seq.foldBack
        let initState = unit Seq.empty
        let folder x state = prepend <!> (f x) <*> state
        foldBack folder m initState // right

    let traverseSeqM f m =
        let prepend x y = Seq.append (Seq.singleton x) y
        let foldBack = Seq.foldBack
        let initState = unit Seq.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the option

    let traverseListA f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty
        let folder x state = prepend <!> (f x) <*> state
        foldBack folder m initState // right fold over the option

    let traverseListM f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the option

    let inline sequenceSeqA m = traverseSeqA id m
    let inline sequenceSeqM m = traverseSeqM id m
    let inline sequenceListA m = traverseListA id m
    let inline sequenceListM m = traverseListM id m


    let opt (parser: Parser<'a>) : Parser<'a option> =
        let some = parser |>> Some
        let none = unit None
        some <|> none <?> $"(optional %s{parser.Label})"

    let manyN (n: int) (parser: Parser<'a>) : Parser<'a list> =
        { Parse = runNOrMoreOnInput n parser
          Label = $"(%i{n}+ %s{parser.Label})" }

    let many (parser: Parser<'a>) : Parser<'a list> = manyN 0 parser

    let exactlyN (n: int) (parser: Parser<'a>) : Parser<'a list> =
        { Parse = runNOnInput n parser
          Label = $"(exactly %i{n} %s{parser.Label})" }

    let choice (parsers: Parser<'a> seq) : Parser<'a> =
        Seq.reduce orElse parsers
        <?> $"(choice of %A{parsers |> Seq.map getLabel |> Seq.toList})"

    let between (p1: Parser<'a>) (p2: Parser<'b>) (p3: Parser<'c>) : Parser<'b> =
        p1 >>. p2 .>> p3
        <?> $"(%s{p2.Label} between %s{p1.Label} and %s{p3.Label})"

    let sepByN (n: int) (phead: Parser<'a>) (separator: Parser<'b>) : Parser<'a list> =
        let toList (head, tail) = head :: tail
        let ptail = separator >>. phead

        phead .>>. manyN n ptail |>> toList <|> unit []
        <?> $"(%s{phead.Label} separated by %s{separator.Label})"

    let sepBy (value: Parser<'a>) (separator: Parser<'b>) : Parser<'a list> = sepByN 0 value separator

    let pchar (charToMatch: char) : Parser<char> =
        satisfy ((=) charToMatch) $"{charToMatch}"

    let pcharsN (n: int) (charToMatch: char) : Parser<string> =
        manyN n (pchar charToMatch) |>> String.ofChars

    let pchars (charToMatch: char) : Parser<string> = pcharsN 0 charToMatch

    let pstring (strToMatch: string) : Parser<string> =
        strToMatch |> traverseSeqA pchar
        |>> String.ofChars
        <?> "string"

    let anyOf (chars: char seq) : Parser<char> =
        chars |> Seq.map pchar |> choice
        <?> $"(any of %A{chars})"

    let pletter : Parser<char> = satisfy Char.IsLetter "letter"

    let pdigit : Parser<char> = satisfy Char.IsDigit "digit"

    let pdigitsN (n: int) : Parser<string> =
        manyN n pdigit |>> String.ofChars <?> "digits"

    let pdigits : Parser<string> = pdigitsN 0

    let phexdigit : Parser<char> =
        let hexLetter =
            [ yield! [ 'a' .. 'f' ]
              yield! [ 'A' .. 'F' ] ]

        anyOf hexLetter <|> pdigit

    let minus : Parser<char> = pchar '-'

    let plus : Parser<char> = pchar '+'

    let zero : Parser<char> = pchar '0'

    let period : Parser<char> = pchar '.'

    let nonZero : Parser<char> =
        let isNonZeroDigit c = Char.IsDigit c && c <> '0'
        satisfy isNonZeroDigit "non-zero digit"

    let pint : Parser<int> =
        let sign = minus <|> plus

        let toInt (optSign, digits) =
            let sign = optSign |> Option.defaultValue '+'
            int $"%c{sign}%s{digits}"

        opt sign .>>. pdigitsN 1 |>> toInt <?> "int"

    let pfloat : Parser<float> =
        let sign = minus <|> plus
        let frac = period >>. pdigitsN 1
        let exp = anyOf [ 'e'; 'E' ] >>. pint

        let toSign = Option.defaultValue '+'

        let toFloat (((optSign, digits), optFrac), optExp) =
            let sign = optSign |> toSign
            let frac = optFrac |> Option.defaultValue "0"
            let exp = optExp |> Option.defaultValue 0
            float $"%c{sign}%s{digits}.%s{frac}e%d{exp}"

        opt sign
        .>>. pdigitsN 1
        .>>. opt frac
        .>>. opt exp
        |>> toFloat
        <?> "float"

    let whitespace : Parser<char> = satisfy Char.IsWhiteSpace "whitespace"

    let whitespacesN (n: int) : Parser<string> =
        manyN n whitespace |>> String.ofChars
        <?> "whitespaces"

    let whitespaces : Parser<string> = whitespacesN 0

type Parser<'a> with
    static member inline (<?>)(parser, label) = Parser.op_LessQmarkGreater parser label
    static member inline (.>>.)(p1, p2) = Parser.op_DotGreaterGreaterDot p1 p2
    static member inline (<|>)(p1, p2) = Parser.op_LessBarGreater p1 p2
    static member inline (<<|)(f, p) = Parser.op_LessLessBar f p
    static member inline (|>>)(p, f) = Parser.op_BarGreaterGreater f p
    static member inline (.>>)(p1, p2) = Parser.op_DotGreaterGreater p1 p2
    static member inline (>>.)(p1, p2) = Parser.op_GreaterGreaterDot p1 p2
    static member inline (>>%)(p, x) = Parser.op_GreaterGreaterPercent p x
    static member inline (<*)(x, y) = Parser.op_LessMultiply x y
    static member inline ( *> )(x, y) = Parser.op_MultiplyGreater x y
    static member inline (<!>)(f, m) = Parser.op_LessBangGreater f m
    static member inline (<*>)(fm, m) = Parser.op_LessMultiplyGreater fm m
    static member inline (=<<)(f, m) = Parser.op_EqualsLessLess f m
    static member inline (>>=)(m, f) = Parser.op_GreaterGreaterEquals m f
    static member inline (>=>)(f1, f2) = Parser.op_GreaterEqualsGreater f1 f2
    static member inline (<=<)(f2, f1) = Parser.op_LessEqualsLess f2 f1
