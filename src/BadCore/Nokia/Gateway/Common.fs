namespace BadCore.Nokia.Gateway

open System
open BadCore.IO.Net
open BadCore.Text.Json

type Error =
    | HttpError of HttpError
    | AuthError of string
    | ResponseError of JsonElementError

[<AutoOpen>]
module Common =
    let createUri (resource: string): Uri =
        Uri $"http://192.168.12.1{resource}"


