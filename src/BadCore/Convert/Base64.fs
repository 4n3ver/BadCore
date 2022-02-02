﻿namespace BadCore.Convert

type Base64 = private Base64 of string

module Base64 =
    open System

    let encode (bytes: byte array): Base64 =
        Convert.ToBase64String bytes |> Base64

    let decode (Base64 encoded: Base64): byte array =
        Convert.FromBase64String encoded

    let toString (Base64 encoded: Base64) = encoded

    let parse (base64String: string): Result<byte array, FormatError> =
        try
            base64String |> Base64 |> decode |> Ok
        with
        | :? FormatException as ex -> Error(FormatError ex)
