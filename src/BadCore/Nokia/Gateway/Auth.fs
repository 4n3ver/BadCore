namespace BadCore.Nokia.Gateway

open System.Text
open BadCore.Extensions
open BadCore.Collections
open BadCore.Control
open BadCore.Convert
open BadCore.IO.Net
open BadCore.Security.Cryptography
open BadCore.Text.Json
open BadCore.Nokia

module Auth =
    [<Struct>]
    type Token =
        { Result: bool
          Sid: string
          Token: string }

    [<Struct>]
    type Nonce =
        { Iterations: int
          Nonce: string
          RandomKey: string
          PublicKey: string }

    [<Struct>]
    type Form = private Form of (string * string) list

    module Form =
        let private urlEscape =
            let escape =
                function
                | '+' -> '-'
                | '/' -> '_'
                | '=' -> '.'
                | c -> c

            Seq.map escape >> String.ofChars

        let private iterate iterations (value: string) =
            let rec iter i bytes =
                if i < iterations then
                    iter (i + 1) (SHA256.hash bytes)
                else
                    (bytes |> Hex.encode |> Hex.toString).ToLower()

            if iterations >= 1 then
                iter 0 (value |> Encoding.UTF8.GetBytes)
            else
                value.ToLower()

        let create (username: string) (password: string) (nonce: Nonce) : Form =
            let hash str1 str2 =
                $"%s{str1}:%s{str2}"
                |> Encoding.UTF8.GetBytes
                |> SHA256.hash
                |> Base64.encode
                |> Base64.toString

            let randomByteString =
                RandomNumberGenerator.getBytes
                >> Base64.encode
                >> Base64.toString

            let password = password |> iterate nonce.Iterations

            let credentialHash = hash username password

            let form =
                [ ("userhash", hash username nonce.Nonce)
                  ("RandomKeyhash", hash nonce.RandomKey nonce.Nonce)
                  ("response", hash credentialHash nonce.Nonce)
                  ("nonce", nonce.Nonce)
                  ("enckey", randomByteString 16)
                  ("enciv", randomByteString 16) ]

            form |> List.map (Pair.mapSnd urlEscape) |> Form

    let getNonce () : AsyncResult<Nonce, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals

        let parseResponse json =
            Ok
                { Iterations =
                      json
                      |> JsonElement.getProperty "iterations"
                      |> JsonElement.getInt
                  Nonce =
                      json
                      |> JsonElement.getProperty "nonce"
                      |> JsonElement.getString
                  RandomKey =
                      json
                      |> JsonElement.getProperty "randomKey"
                      |> JsonElement.getString
                  PublicKey =
                      json
                      |> JsonElement.getProperty "pubkey"
                      |> JsonElement.getString }

        let getResponseBody =
            JsonElement.deserialize
            >> Result.mapError ResponseError
            >> Result.bind parseResponse
            >> Async.unit

        let get =
            Http.getString >> AsyncResult.mapError HttpError

        get (createUri "/login_web_app.cgi?nonce")
        >>= getResponseBody

    let getToken (Form form: Form) : AsyncResult<Token, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals

        let parseResponse response json =
            let result =
                json
                |> JsonElement.getProperty "result"
                |> JsonElement.getInt
                |> (=) 0

            if result then
                Ok
                    { Result = result
                      Sid =
                          json
                          |> JsonElement.getProperty "sid"
                          |> JsonElement.getString
                      Token =
                          json
                          |> JsonElement.getProperty "token"
                          |> JsonElement.getString }
            else
                Error(AuthError $"%A{response}")

        let getResponseBody response =
            response
            |> HttpContent.readContentAsJson
            |> AsyncResult.mapError ResponseError
            |> Async.map (Result.bind (parseResponse response))

        let post uri =
            Http.post uri >> AsyncResult.mapError HttpError

        post (createUri "/login_web_app.cgi") (HttpContent.urlEncodedForm form)
        >>= getResponseBody

    let authenticate (username: string) (password: string) : AsyncResult<Token, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals

        let getToken =
            Form.create username password >> getToken

        getNonce () >>= getToken
