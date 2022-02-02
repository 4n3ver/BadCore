namespace BadCore.Convert

open System

type Hex = private Hex of string

module Hex =
    open System

    let encode (bytes: byte array): Hex =
        Convert.ToHexString bytes |> Hex

    let decode (Hex encoded: Hex): byte array =
        Convert.FromHexString encoded

    let toString (Hex encoded: Hex) = encoded

    let parse (hexString: string): Result<byte array, FormatError> =
        try
            hexString |> Hex |> decode |> Ok
        with
        | :? FormatException as ex -> Error(FormatError ex)
