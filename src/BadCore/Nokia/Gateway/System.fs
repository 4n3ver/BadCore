namespace BadCore.Nokia.Gateway

open System
open System.Net.Http
open BadCore.Extensions
open BadCore.Control
open BadCore.IO.Net
open BadCore.Nokia

module System =
    let hasInternetAccess (): Async<bool> =
        Uri "https://example.org"
        |> Http.getString
        |> AsyncResult.isOk

    let reboot (authToken: Auth.Token): AsyncResult<string, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals
        let request = new HttpRequestMessage()
        request.Method <- HttpMethod.Post
        request.RequestUri <- createUri "/reboot_web_app.cgi"
        request.Content <- Http.Content.urlEncodedForm [ ("csrf_token", authToken.Token) ]
        request.Headers.Add("Cookie", [ $"sid={authToken.Sid}; lsid={authToken.Lsid}" ])

        let getResponseBody response =
            response
            |> Http.Content.readContentAsString
            |> Async.map Result.unit

        let send =
            Http.send
            >> AsyncResult.mapError HttpError

        send request >>= getResponseBody
