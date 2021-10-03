namespace BadCore.Extensions

module Array2D =
    let flatten (nested: 'a array array) : 'a [,] =
        let height = Array.length nested

        let width =
            if height > 0 then
                Array.length nested.[0]
            else
                0

        Array2D.init height width (fun y x -> nested.[y].[x])

    let rows (arr: 'a [,]) : 'a array seq =
        seq { for r in 0 .. (Array2D.length1 arr - 1) -> arr.[r, *] }

    let cols (arr: 'a [,]) : 'a array seq =
        seq { for c in 0 .. (Array2D.length2 arr - 1) -> arr.[*, c] }

    module Parallel =
        let map (f: 'a -> 'b) (arr: 'a [,]) : 'b [,] =
            let height = Array2D.length1 arr
            let width = Array2D.length2 arr
            let res = Array2D.zeroCreate height width

            if (height > 0 && width > 0) then
                let setRow y =
                    async {
                        let set x v = res.[y, x] <- (f v)
                        Array.Parallel.iteri set arr.[y, *]
                    }

                Seq.init height id
                |> Seq.map setRow
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

            res
