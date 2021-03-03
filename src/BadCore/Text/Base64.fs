namespace BadCore.Text

type Base64 = private Base64 of string

module Base64 =
    open System

    let encode (bytes: byte array): Base64 =
        Convert.ToBase64String bytes |> Base64

    let toString (Base64 encoded: Base64) = encoded

