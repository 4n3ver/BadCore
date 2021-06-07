namespace BadCore.Collections

module Pair =
    let create (x: 'a) (y: 'b): 'a * 'b = x, y
    let swap ((x, y): 'a * 'b): 'b * 'a = y, x
    let mapFst (f: 'a -> 'p) ((x, y): 'a * 'b): 'p * 'b = f x, y
    let mapSnd (f: 'b -> 'q) ((x, y): 'a * 'b): 'a * 'q = x, f y
