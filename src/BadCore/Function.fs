namespace BadCore

module Function =
    open System.Collections.Generic

    let flip (f: 'a -> 'b -> 'c) (b: 'b) (a: 'a): 'c = f a b

    let memoize (f: 'a -> 'b): 'a -> 'b when 'a: comparison =
        let cache = new Dictionary<_, _>()

        fun x ->
            match cache.TryGetValue x with
            | true, res -> res
            | _ ->
                let res = f x
                cache.Add(x, res)
                res
