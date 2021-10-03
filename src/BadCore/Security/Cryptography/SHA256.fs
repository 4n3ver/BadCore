namespace BadCore.Security.Cryptography

open System.Security.Cryptography

module SHA256 =
    let hash (bytes: byte array): byte array =
        use sha256 = SHA256.Create()
        sha256.ComputeHash bytes
