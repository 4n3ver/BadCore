namespace BadCore.IO.Net

module Http =
    open System
    open System.Net.Http
    open BadCore.Control
    open BadCore.Extensions

    let client = lazy (new HttpClient())

    let getString (uri: Uri): AsyncResult<string, HttpRequestException> =
        async {
            printfn "GET: %s" uri.AbsoluteUri

            try
                let! resp =
                    uri
                    |> client.Force().GetStringAsync
                    |> Async.AwaitTask

                return Ok resp
            with :? HttpRequestException as ex -> return Error ex
        }

    let parseUri (uri: string): Result<Uri, UriFormatException> =
        try
            uri |> Uri |> Ok
        with :? UriFormatException as ex -> Error ex

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
            with ex -> return Error ex
        }
