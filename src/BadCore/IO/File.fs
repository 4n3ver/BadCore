namespace BadCore.IO

module File =
    open System
    open System.IO
    open System.Security

    type Error =
        | ArgumentError of ArgumentException
        | DirectoryNotFound of DirectoryNotFoundException
        | FileNotFound of FileNotFoundException
        | IOError of IOException
        | PathTooLong of PathTooLongException
        | SecurityError of SecurityException
        | UnauthorizedAccess of UnauthorizedAccessException

    let readLines (filePath: string): Result<string seq, Error> =
        try
            let lines = File.ReadLines filePath
            let lineSeq = seq { yield! lines }
            Ok lineSeq
        with
        | :? ArgumentException as ex -> Error(ArgumentError ex)
        | :? PathTooLongException as ex -> Error(PathTooLong ex)
        | :? DirectoryNotFoundException as ex -> Error(DirectoryNotFound ex)
        | :? FileNotFoundException as ex -> Error(FileNotFound ex)
        | :? IOException as ex -> Error(IOError ex)
        | :? SecurityException as ex -> Error(SecurityError ex)
        | :? UnauthorizedAccessException as ex -> Error(UnauthorizedAccess ex)
