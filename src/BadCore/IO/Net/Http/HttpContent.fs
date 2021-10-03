namespace BadCore.IO.Net

open System.Text
open System.Net.Http

module HttpContent =
    let urlEncodedForm (form: (string * string) seq) : StringContent =
        let toString (key, value) = $"{key}={value}"

        let form =
            form |> Seq.map toString |> String.concat "&"

        new StringContent(form, Encoding.UTF8, "application/x-www-form-urlencoded")

    let readContentAsString (response: HttpResponseMessage) : Async<string> =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
