namespace BadCore.Nokia.Gateway

open System.Net.Http
open System.Text
open BadCore.Extensions
open BadCore.Collections
open BadCore.Control
open BadCore.Text
open BadCore.IO.Net
open BadCore.Security.Cryptography
open BadCore.Text.Json
open BadCore.Nokia

module Auth =
    [<Struct>]
    type Token =
        { Result: bool
          Sid: string
          Lsid: string
          Token: string
          IsCtcAdmin: bool
          PasswordChangeSecureFlag: bool }

    [<Struct>]
    type Nonce =
        { Nonce: string
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

        let create (username: string) (password: string) (nonce: Nonce): Form =
            let hash (str: string) =
                Encoding.UTF8.GetBytes str
                |> SHA256.hash
                |> Base64.encode
                |> Base64.toString

            let randomByteString =
                RandomNumberGenerator.getBytes
                >> Base64.encode
                >> Base64.toString

            let credentialHash = hash $"{username}:{password}"

            let form =
                [ ("userhash", hash $"{username}:{nonce.Nonce}")
                  ("RandomKeyhash", hash $"{nonce.RandomKey}:{nonce.Nonce}")
                  ("response", hash $"{credentialHash}:{nonce.Nonce}")
                  ("nonce", nonce.Nonce)
                  ("enckey", randomByteString 16)
                  ("enciv", randomByteString 16) ]

            form |> List.map (Pair.mapSnd urlEscape) |> Form

    let getNonce (): AsyncResult<Nonce, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals

        let parseResponse (body: string) =
            let json = JsonElement.deserialize body

            Ok
                { Nonce =
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

        let getResponseBody = parseResponse >> Async.unit

        let get =
            Http.getString
            >> AsyncResult.mapError HttpError

        get (createUri "/login_web_app.cgi?nonce")
        >>= getResponseBody

    let getToken (Form form: Form): AsyncResult<Token, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals

        let parseResponse (response: HttpResponseMessage) (body: string) =
            let json = JsonElement.deserialize body

            let result =
                json.GetProperty("result").GetInt32() = 0

            let lsidCookie =
                HttpCookies.create response
                |> HttpCookies.get "lsid"
                |> Option.get

            if result then
                Ok
                    { Result = result
                      Lsid = lsidCookie.Value
                      Sid =
                          json
                          |> JsonElement.getProperty "sid"
                          |> JsonElement.getString
                      Token =
                          json
                          |> JsonElement.getProperty "token"
                          |> JsonElement.getString
                      IsCtcAdmin =
                          json
                          |> JsonElement.getProperty "pswd_chng_scr_flg"
                          |> JsonElement.getInt = 1
                      PasswordChangeSecureFlag =
                          json
                          |> JsonElement.getProperty "is_ctc_admin"
                          |> JsonElement.getInt = 1 }
            else
                Error(AuthError(sprintf "%A" response))

        let getResponseBody response =
            response
            |> HttpContent.readContentAsString
            |> Async.map (parseResponse response)

        let post uri =
            Http.post uri
            >> AsyncResult.mapError HttpError

        post (createUri "/login_web_app.cgi") (HttpContent.urlEncodedForm form)
        >>= getResponseBody

    let authenticate (username: string) (password: string): AsyncResult<Token, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals

        let getToken =
            Form.create username password >> getToken

        getNonce () >>= getToken
