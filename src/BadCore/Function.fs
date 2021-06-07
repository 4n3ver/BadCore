namespace BadCore

open System.Collections.Generic

module Function =
    let constant (x: 'a) (_: 'b): 'a = x
    let flip (f: 'a -> 'b -> 'c) (b: 'b) (a: 'a): 'c = f a b
    let uncurry (f: 'a -> 'b -> 'r) ((x, y): 'a * 'b): 'r = f x y

    let memoize (f: 'a -> 'b): 'a -> 'b when 'a: comparison =
        let cache = Dictionary<_, _>()

        fun x ->
            match cache.TryGetValue x with
            | true, res -> res
            | _ ->
                let res = f x
                cache.Add(x, res)
                res
