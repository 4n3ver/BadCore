namespace BadCore

open System

type ErrorMessage = string
type ParserLabel = string
type ParserError = { Label: ParserLabel; Messages: ErrorMessage list }

[<Struct>]
type Parser<'a> =
    private { Parse: string -> Result<'a * string, ParserError>; Label: ParserLabel }

module Parser =
    open BadCore.Extensions

    let private printResult (result: Result<'a * string, ParserError>): unit =
        match result with
        | Ok (value, _) ->
            printfn "%A" value
        | Error err ->
            printfn "Error parsing %s\n%A" err.Label err.Messages

    let run (parser: Parser<'a>) (str: string): Result<'a * string, ParserError> = parser.Parse str

    let runN (n: int) (parser: Parser<'a>) (str: string): Result<'a list * string, ParserError> =
        let rec parse n (parsed, str) =
            if (n > 0) then
                match run parser str with
                | Ok (value, rest) -> parse (n - 1) (value :: parsed, rest)
                | Error err -> Error err
            else
                Ok(List.rev parsed, str)

        parse n ([], str)

    let runNOrMore (n: int) (parser: Parser<'a>) (str: string): Result<'a list * string, ParserError> =
        let rec parse n (parsed, str) =
            match run parser str with
            | Ok (value, rest) -> parse (n - 1) (value :: parsed, rest)
            | Error _ when n <= 0 -> Ok(List.rev parsed, str)
            | Error err -> Error err

        parse n ([], str)

    let runZeroOrMore (parser: Parser<'a>) (str: string): Result<'a list * string, ParserError> =
        runNOrMore 0 parser str

    let setLabel (parser: Parser<'a>) (label: ParserLabel): Parser<'a> =
        let parser str =
            match run parser str with
            | Ok res -> Ok res
            | Error err -> Error ({ err with Label = label })

        { Parse = parser; Label = label }

    let andThen (parser1: Parser<'a>) (parser2: Parser<'b>): Parser<'a * 'b> =
        let (>>=) = Result.op_GreaterGreaterEquals

        let parser str =
            run parser1 str
            >>= (fun (parsed1, rest) ->
                run parser2 rest
                >>= (fun (parsed2, rest) -> Ok((parsed1, parsed2), rest)))

        { Parse = parser; Label = "unknown" }

    let orElse (parser1: Parser<'a>) (parser2: Parser<'a>): Parser<'a> =
        let parser str =
            match run parser1 str with
            | Error _ -> run parser2 str
            | res -> res

        { Parse = parser; Label = "unknown" }

    let unit (x: 'a): Parser<'a> = { Parse = (fun rest -> Ok(x, rest)); Label = "unknown" }

    let bind (f: 'a -> Parser<'b>) (m: Parser<'a>): Parser<'b> =
        let parser str =
            match run m str with
            | Ok (parsed, rest) -> run (f parsed) rest
            | Error err -> Error err

        { Parse = parser; Label = "unknown" }

    let map (fn: 'a -> 'b) (parser: Parser<'a>): Parser<'b> =
        let map (parsed, rest) = fn parsed, rest
        { Parse = (run parser >> Result.map map); Label = "unknown" }

    let apply (fm: Parser<'a -> 'b>) (m: Parser<'a>): Parser<'b> =
        m |> andThen fm |> map (fun (fn, p) -> fn p)

    let ( <?> ) = setLabel
    let (.>>.) = andThen
    let (<|>) = orElse
    let (|>>) x f = map f x
    let (.>>) p1 p2 = p1 .>>. p2 |>> (fun (a, _) -> a)
    let (>>.) p1 p2 = p1 .>>. p2 |>> (fun (_, b) -> b)

    // Derived
    let (<!>) = map
    let (<*>) = apply
    let (=<<) = bind
    let (>>=) m f = f =<< m
    let (>=>) f1 f2 = bind f2 << f1
    let (<=<) f2 f1 = f1 >=> f2
    let lift1 = (<!>)
    let lift2 f m1 m2 = f <!> m1 <*> m2
    let lift3 f m1 m2 m3 = f <!> m1 <*> m2 <*> m3
    let (<*) x y = lift2 (fun l _ -> l) x y
    let ( *> ) x y = lift2 (fun _ r -> r) x y

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

    let sequenceSeqA m = traverseSeqA id m
    let sequenceSeqM m = traverseSeqM id m
    let sequenceListA m = traverseListA id m
    let sequenceListM m = traverseListM id m

    let opt (parser: Parser<'a>): Parser<'a option> =
        let some = parser |>> Some
        let none = unit None
        some <|> none

    let many (parser: Parser<'a>): Parser<'a list> = { Parse = runZeroOrMore parser; Label = "unknown" }
    let manyN (n: int) (parser: Parser<'a>): Parser<'a list> = { Parse = runNOrMore n parser; Label = "unknown" }
    let choice (parsers: Parser<'a> seq): Parser<'a> = Seq.reduce orElse parsers
    let between p1 p2 p3 = p1 >>. p2 .>> p3

    let satisfy (predicate: char -> bool) (label: ParserLabel) =
        let parser (str: string) =
            if (Seq.isEmpty str) then
                Error({ Messages = ["No more input!"]; Label = label })
            elif (predicate str.[0]) then
                Ok(str.[0], str.[1..])
            else
                Error({ Messages = [$"Unexpected '{str.[0]}'!"]; Label = label })

        { Parse = parser; Label = label }

    let pchar (charToMatch: char): Parser<char> = satisfy ((=) charToMatch) $"{charToMatch}"

    let anyOf (chars: char seq): Parser<char> =
        chars
        |> Seq.map pchar
        |> choice
        <?> sprintf "any of %A" chars

    let pstring (strToMatch: string): Parser<string> =
        strToMatch
        |> traverseSeqA pchar
        |> map (Seq.toArray >> String)
        <?> "string"

    let pint: Parser<int> =
        let toInt (sign, digits) =
            let digits = digits |> Seq.toArray |> String |> int

            match sign with
            | Some _ -> -digits
            | _ -> digits

        let digit = satisfy Char.IsDigit "digit"
        let digits = manyN 1 digit
        let maybeNegative = opt (pchar '-')
        maybeNegative .>>. digits |>> toInt <?> "int"
