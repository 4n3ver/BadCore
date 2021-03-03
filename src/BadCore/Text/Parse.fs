namespace BadCore.Text

open System
open System.Numerics
open BadCore.Extensions

module Parse =
    let private tryParse fn str =
        try
            Ok(fn str)
        with :? FormatException as ex -> Error ex

    let parseInt (str: string): Result<int, FormatException> = tryParse int str

    let parseUInt64 (str: string): Result<uint64, FormatException> = tryParse uint64 str

    let parseBigInt (str: string): Result<bigint, FormatException> = tryParse BigInteger.Parse str

    let parseIntBinary (str: string): Result<int, FormatException> =
        tryParse (fun s -> Convert.ToInt32(s, 2)) str

    let parseUInt64Binary (str: string): Result<uint64, FormatException> =
        tryParse (fun s -> Convert.ToUInt64(s, 2)) str

    module Pattern =
        let (|Integer|_|) (input: string): int option = input |> parseInt |> Result.ignoreError

        let (|UnsignedInt64|_|) (input: string): uint64 option =
            input |> parseUInt64 |> Result.ignoreError

        let (|BigInteger|_|) (input: string): bigint option =
            input |> parseBigInt |> Result.ignoreError

        let (|IntBinary|_|) (input: string): int option =
            input |> parseIntBinary |> Result.ignoreError

        let (|UnsignedInt64Binary|_|) (input: string): uint64 option =
            input |> parseUInt64Binary |> Result.ignoreError
