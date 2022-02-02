namespace BadCore.Security.Cryptography

module SHA256 =
    open System.Security.Cryptography

    let hash (bytes: byte array): byte array =
        use sha256 = SHA256.Create()
        bytes |> sha256.ComputeHash

