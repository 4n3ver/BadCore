namespace BadCore.Text.Json

open System
open System.Text.Json

type JsonElementError =
    | InvalidJsonError of JsonException

module private JsonElementError =
    let from (ex: Exception) : JsonElementError =
        match ex with
        | :? JsonException as ex -> InvalidJsonError ex
        | _ -> raise ex
