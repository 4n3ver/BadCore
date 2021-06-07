namespace BadCore.Nokia.Gateway

open System
open System.Net.Http
open BadCore.Extensions
open BadCore.Control
open BadCore.IO.Net
open BadCore.Nokia

module System =
    let hasInternetAccess () : Async<bool> =
        [ "https://example.org"
          "https://httpstat.us/200"
          "https://1.1.1.1" ]
        |> Seq.map Uri
        |> Seq.map Http.head
        |> AsyncSeq.tryFind Result.isOk
        |> Async.map Option.isSome


    let reboot (authToken: Auth.Token) : AsyncResult<string, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals
        let request = new HttpRequestMessage()
        request.Method <- HttpMethod.Post
        request.RequestUri <- createUri "/reboot_web_app.cgi"
        request.Content <- HttpContent.urlEncodedForm [ ("csrf_token", authToken.Token) ]
        request.Headers.Add("Cookie", [ $"sid={authToken.Sid}; lsid={authToken.Lsid}" ])

        let getResponseBody response =
            response
            |> HttpContent.readContentAsString
            |> Async.map Ok

        let send =
            Http.send >> AsyncResult.mapError HttpError

        send request >>= getResponseBody
