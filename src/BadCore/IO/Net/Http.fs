namespace BadCore.IO.Net

open System
open System.Collections.Generic
open System.Net.Http
open System.Text
open System.Threading.Tasks
open BadCore.Extensions
open BadCore.Control
open BadCore.Collections

module Http =
    type Error =
        | RequestError of HttpRequestException
        | TimeoutError of TaskCanceledException
        | UnknownError of Exception

    module private Error =
        let from (ex: Exception): Error =
            match ex with
            | :? HttpRequestException as ex -> RequestError ex
            | :? TaskCanceledException as ex -> TimeoutError ex
            | _ -> raise ex

    type Cookie =
        { Key: string
          Value: string
          Path: string option
          Expires: string option }

    module private Cookie =
        let create (props: (string * string) seq): Cookie =
            let getAttributeValue attrKey =
                props
                |> Seq.tryFind (Pair.value1 >> (=) attrKey)
                |> Option.map Pair.value2

            let (key, value) = Seq.head props
            let path = getAttributeValue "Path"
            let expires = getAttributeValue "Expires"

            { Key = key
              Value = value
              Path = path
              Expires = expires }

        let parse (cookieVal: string): Cookie =
            let parseKVPair (str: string) =
                match str.Split '=' with
                | [| key; value |] -> key, value
                | [| key |] -> key, "true"
                | _ -> failwithf "Unexpected cookie format %A" str

            cookieVal.Split "; "
            |> Seq.map parseKVPair
            |> create

    type Cookies = private Cookies of Map<string, Cookie>

    module Cookies =
        type private HeaderEntry = KeyValuePair<string, string seq>

        let create (response: HttpResponseMessage): Cookies =
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
            |> Cookies

        let get (key: string) (Cookies cookies: Cookies): Cookie option = cookies |> Map.tryFind key


    let private client = lazy (new HttpClient())

    let inline private awaitHttpRequest request =
        async {
            try
                // Exceptions are wrapped in AggregateException following the convention of the Task Parallel Library
                let! resp = request |> Async.AwaitTask
                return Ok resp
            with :? AggregateException as ex -> return Error(Error.from ex.InnerException)
        }

    let send (request: HttpRequestMessage): AsyncResult<HttpResponseMessage, Error> =
        async {
            printfn $"{request.Method}: {request.RequestUri.AbsoluteUri}"

            return!
                request
                |> client.Force().SendAsync
                |> awaitHttpRequest
        }

    let get (uri: Uri): AsyncResult<HttpResponseMessage, Error> =
        async {
            printfn "GET: %s" uri.AbsoluteUri

            return! uri |> client.Force().GetAsync |> awaitHttpRequest
        }

    let getString (uri: Uri): AsyncResult<string, Error> =
        async {
            printfn "GET: %s" uri.AbsoluteUri

            return!
                uri
                |> client.Force().GetStringAsync
                |> awaitHttpRequest
        }

    let post (uri: Uri) (content: HttpContent): AsyncResult<HttpResponseMessage, Error> =
        async {
            printfn "POST: %s" uri.AbsoluteUri

            return!
                (uri, content)
                |> client.Force().PostAsync
                |> awaitHttpRequest
        }

    let parseUri (uri: string): Result<Uri, UriFormatException> =
        try
            uri |> Uri |> Ok
        with :? UriFormatException as ex -> Result.Error ex

    let downloadToFile (targetPath: string) (uri: Uri): AsyncResult<unit, Exception> =
        async {
            let fileName =
                uri.Segments |> Seq.cast<string> |> Seq.last

            let targetFilePath = sprintf "%s/%s" targetPath fileName

            printfn "Downloading: %s into %s" uri.AbsoluteUri targetFilePath

            try
                use! stream =
                    uri
                    |> client.Force().GetStreamAsync
                    |> Async.AwaitTask

                use outputFile =
                    new IO.FileStream(targetFilePath, IO.FileMode.Create)

                do! stream.CopyToAsync outputFile |> Async.AwaitTask

                return Ok()
            with ex -> return Result.Error ex
        }

    module Content =
        let urlEncodedForm (form: (string * string) seq): StringContent =
            let toString (key, value) = $"{key}={value}"

            let form =
                form |> Seq.map toString |> String.concat "&"

            new StringContent(form, Encoding.UTF8, "application/x-www-form-urlencoded")

        let readContentAsString (response: HttpResponseMessage): Async<string> =
            response.Content.ReadAsStringAsync()
            |> Async.AwaitTask
