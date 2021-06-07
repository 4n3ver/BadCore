namespace BadCore.IO.Net

open System
open System.Net.Http
open System.Threading.Tasks
open BadCore.Extensions
open BadCore.Control

module Http =
    let private client = lazy (new HttpClient())

    let inline private awaitHttpRequest request =
        async {
            try
                // Exceptions are wrapped in AggregateException following the convention of the Task Parallel Library
                let! resp = request |> Async.AwaitTask
                return Ok resp
            with
            | :? AggregateException as ex -> return Error(HttpError.from ex.InnerException)
            | :? TaskCanceledException as ex -> return Error(HttpError.from ex)
        }

    let send (request: HttpRequestMessage) : AsyncResult<HttpResponseMessage, HttpError> =
        async {
            printfn $"{request.Method}: {request.RequestUri.AbsoluteUri}"

            return!
                request
                |> client.Force().SendAsync
                |> awaitHttpRequest
        }

    let head (uri: Uri) : AsyncResult<HttpResponseMessage, HttpError> =
        async {
            let request =
                new HttpRequestMessage(HttpMethod.Head, uri)

            return! send request
        }

    let get (uri: Uri) : AsyncResult<HttpResponseMessage, HttpError> =
        async {
            printfn $"GET: %s{uri.AbsoluteUri}"

            return! uri |> client.Force().GetAsync |> awaitHttpRequest
        }

    let getString (uri: Uri) : AsyncResult<string, HttpError> =
        async {
            printfn $"GET: %s{uri.AbsoluteUri}"

            return!
                uri
                |> client.Force().GetStringAsync
                |> awaitHttpRequest
        }

    let post (uri: Uri) (content: HttpContent) : AsyncResult<HttpResponseMessage, HttpError> =
        async {
            printfn $"POST: %s{uri.AbsoluteUri}"

            return!
                (uri, content)
                |> client.Force().PostAsync
                |> awaitHttpRequest
        }

    let parseUri (uri: string) : Result<Uri, UriFormatException> =
        try
            uri |> Uri |> Ok
        with :? UriFormatException as ex -> Result.Error ex

    let downloadToFile (targetPath: string) (uri: Uri) : AsyncResult<unit, Exception> =
        async {
            let fileName =
                uri.Segments |> Seq.cast<string> |> Seq.last

            let targetFilePath = $"%s{targetPath}/%s{fileName}"

            printfn $"Downloading: %s{uri.AbsoluteUri} into %s{targetFilePath}"

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


