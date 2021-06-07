namespace BadCore.Text.Json

open BadCore
open BadCore.Extensions
open BadCore.Text
open BadCore.Text.Parser

type JsonValue =
    | JsonNull
    | JsonBool of bool
    | JsonString of string
    | JsonNumber of float
    | JsonArray of JsonValue array
    | JsonObject of Map<string, JsonValue>

module JsonParser =
    [<AutoOpen>]
    module private Internal =
        // WTF#!
        let inline createParserForwardedToRef<'a> () =
            // ref to placeholder Parser
            let parse _ = failwith "unfixed forwarded parser"
            let implRef = ref { Parse = parse; Label = "stub" }

            // forward input to the placeholder (Note: "!" is the dereferencing operator)
            let parse state = (!implRef).Parse state
            let proxy = { Parse = parse; Label = "proxy" }

            proxy, implRef

        let jsonValue, jsonValueRef =
            createParserForwardedToRef<JsonValue> ()

        let jsonNull = pstring "null" >>% JsonNull <?> "null"

        let jsonBool =
            let jsonTrue = pstring "true" >>% true

            let jsonFalse = pstring "false" >>% false

            jsonTrue <|> jsonFalse |>> JsonBool <?> "bool"

        let jsonNumber = pfloat |>> JsonNumber <?> "number"

        let jsonUnescapedChar =
            let unescapedChar =
                Function.flip Seq.contains [ '\\'; '"' ] >> not

            satisfy unescapedChar "unescaped char"

        let jsonEscapedChar =
            let createParser (matcher, result) = pstring $"\\%c{matcher}" >>% result

            [ '"', '"' // quote
              '\\', '\\' // slash/reverse solidus
              '/', '/' // backslash/solidus
              'b', '\b' // backspace
              'f', '\f' // formfeed
              'n', '\n' // newline
              'r', '\r' // carriage return
              't', '\t' ] // tab
            |> Seq.map createParser
            |> choice
            <?> "escaped char"

        let jsonUnicodeChar =
            let toChar =
                String.ofChars
                >> Parse.parseIntHex
                >> Result.get
                >> char

            pstring "\u" >>. exactlyN 4 phexdigit |>> toChar
            <?> "unicode char"

        let jsonChar =
            choice [ jsonUnescapedChar
                     jsonEscapedChar
                     jsonUnicodeChar ]

        let quotedString =
            let quote = pchar '"'

            quote >>. many jsonChar .>> quote
            |>> String.ofChars

        let jsonString =
            quotedString |>> JsonString <?> "json string"

        let jsonArray =
            let openBracket = pchar '[' .>> whitespaces
            let closingBracket = pchar ']' .>> whitespaces
            let comma = pchar ',' .>> whitespaces
            let value = jsonValue .>> whitespaces
            let values = sepBy value comma

            between openBracket values closingBracket
            |>> (Seq.toArray >> JsonArray)
            <?> "json array"

        let jsonObject =
            let openBrace = pchar '{' .>> whitespaces
            let closingBrace = pchar '}' .>> whitespaces
            let comma = pchar ',' .>> whitespaces
            let colon = pchar ':' .>> whitespaces
            let key = quotedString .>> whitespaces
            let value = jsonValue .>> whitespaces
            let keyValue = (key .>> colon) .>>. value
            let keyValues = sepBy keyValue comma

            between openBrace keyValues closingBrace
            |>> (Map.ofList >> JsonObject)
            <?> "json object"

        jsonValueRef
        := choice [ jsonNull
                    jsonBool
                    jsonString
                    jsonNumber
                    jsonArray
                    jsonObject ]

    let parse (str: string) : ParserResult<JsonValue> = str |> String.trim |> run jsonValue
