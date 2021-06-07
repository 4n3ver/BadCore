namespace BadCore.IO.Net

open System
open System.Net.Http
open System.Threading.Tasks

type HttpError =
    | RequestError of HttpRequestException
    | TimeoutError of TaskCanceledException
    | UnknownError of Exception

module private HttpError =
    let from (ex: Exception) : HttpError =
        match ex with
        | :? HttpRequestException as ex -> RequestError ex
        | :? TaskCanceledException as ex -> TimeoutError ex
        | _ -> raise ex
