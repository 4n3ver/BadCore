namespace BadCore.Collections

module Pair =
    let create (x: 'a) (y: 'b): ('a * 'b) = x, y
    let value1 ((x, _): ('a * 'b)): 'a = x
    let value2 ((_, y): ('a * 'b)): 'b = y
    let swap ((x, y): ('a * 'b)): ('b * 'a) = y, x
    let map (f: 'a -> 'b -> 'r) ((x, y): ('a * 'b)): 'r = f x y
    let map1 (f: 'a -> 'p) ((x, y): ('a * 'b)): ('p * 'b) = f x, y
    let map2 (f: 'b -> 'q) ((x, y): ('a * 'b)): ('a * 'q) = x, f y
