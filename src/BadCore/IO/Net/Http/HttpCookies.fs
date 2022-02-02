namespace BadCore.IO.Net

open System.Net.Http
open System.Collections.Generic

type Cookie =
    { Key: string
      Value: string
      Path: string option
      Expires: string option }

module private Cookie =
    let create (props: (string * string) seq) : Cookie =
        let ignoreCase (attrKey: string) ((key, _): string * string) =
            key.ToLower() = attrKey.ToLower()

        let getAttributeValue attrKey =
            props
            |> Seq.tryFind (ignoreCase attrKey)
            |> Option.map snd

        let (key, value) = Seq.head props
        let path = getAttributeValue "Path"
        let expires = getAttributeValue "Expires"

        { Key = key
          Value = value
          Path = path
          Expires = expires }

    let parse (cookieVal: string) : Cookie =
        let parseKVPair (str: string) =
            match str.Split '=' with
            | [| key; value |] -> key, value
            | [| key |] -> key, "true"
            | _ -> failwithf $"Unexpected cookie format %A{str}"

        cookieVal.Split "; "
        |> Seq.map parseKVPair
        |> create

type HttpCookies = private HttpCookies of Map<string, Cookie>

module HttpCookies =
    type private HeaderEntry = KeyValuePair<string, string seq>

    let create (response: HttpResponseMessage) : HttpCookies =
        let setCookieHeader (header: HeaderEntry) = header.Key = "Set-Cookie"

        let parseCookies (header: HeaderEntry) =
            let toPair cookie = cookie.Key, cookie

            header.Value
            |> Seq.map (Cookie.parse >> toPair)
            |> Map.ofSeq

        response.Headers
        |> Seq.tryFind setCookieHeader
        |> Option.map parseCookies
        |> Option.defaultValue Map.empty
        |> HttpCookies

    let get (key: string) (HttpCookies cookies: HttpCookies) : Cookie option = cookies |> Map.tryFind key

