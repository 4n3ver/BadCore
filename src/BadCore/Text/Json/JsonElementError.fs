namespace BadCore.Text.Json

open System
open System.Text.Json

type JsonElementError =
    | InvalidJsonError of string * JsonException

module private JsonElementError =
    let from (jsonString: string) (ex: Exception) : JsonElementError =
        match ex with
        | :? JsonException as ex -> InvalidJsonError (jsonString, ex)
        | _ -> raise ex
