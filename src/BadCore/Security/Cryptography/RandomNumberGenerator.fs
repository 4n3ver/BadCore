namespace BadCore.Security.Cryptography


module RandomNumberGenerator =
    open System.Security.Cryptography

    let private rng = lazy (RandomNumberGenerator.Create())

    let getBytes count: byte array =
        let bytes = Array.zeroCreate count
        rng.Force().GetBytes(bytes)
        bytes
