namespace BadCore.IO.Net

open System.Text
open System.Text.Json
open System.Net.Http
open BadCore.Extensions
open BadCore.Control
open BadCore.Text.Json

module HttpContent =
    let urlEncodedForm (form: (string * string) seq) : StringContent =
        let toString (key, value) = $"{key}={value}"

        let form =
            form |> Seq.map toString |> String.concat "&"

        new StringContent(form, Encoding.UTF8, "application/x-www-form-urlencoded")

    let readContentAsString (response: HttpResponseMessage) : Async<string> =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask

    let readContentAsJson (response: HttpResponseMessage) : AsyncResult<JsonElement, JsonElementError> =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.map JsonElement.deserialize
