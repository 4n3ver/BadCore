namespace BadCore.Extensions

module Seq =
    // Monad
    let unit (x: 'a) : 'a seq = Seq.singleton x
    let bind (f: 'a -> 'b seq) (m: 'a seq) : 'b seq = Seq.collect f m


    // Functor
    let map (f: 'a -> 'b) (m: 'a seq) : 'b seq = Seq.map f m


    // Applicative
    // [f; g] <*> [x; y] = [f x; f y; g x; g y]
    let apply (fList: ('a -> 'b) seq) (xList: 'a seq) : 'b seq =
        seq {
            for f in fList do
                for x in xList do
                    yield f x
        }


    let initBigInfinite initializer =
        seq {
            let i = ref 0I

            while true do
                yield initializer !i
                i := !i + 1I
        }

    let aggregateBy (key: 'x -> 'k) (combine: 'x seq -> 'v) (seq: 'x seq) : ('k * 'v) seq =
        seq
        |> Seq.groupBy key
        |> Seq.map (fun (x, grouped) -> (x, combine grouped))


    // Derived
    let (<!>) = map
    let (<*>) = apply
    let (=<<) = bind
    let inline (>>=) m f = f =<< m
    let inline (>=>) f1 f2 = bind f2 << f1
    let inline (<=<) f2 f1 = f1 >=> f2
    let lift = (<!>)
    let inline lift2 f m1 m2 = f <!> m1 <*> m2
    let inline lift3 f m1 m2 m3 = f <!> m1 <*> m2 <*> m3
    let inline (<*) x y = lift2 (fun l _ -> l) x y
    let inline ( *> ) x y = lift2 (fun _ -> id) x y

    let traverseSeqA f m =
        let prepend x y = Seq.append (Seq.singleton x) y
        let foldBack = Seq.foldBack
        let initState = unit Seq.empty
        let folder x state = prepend <!> (f x) <*> state
        foldBack folder m initState // right fold over the seq

    let traverseSeqM f m =
        let prepend x y = Seq.append (Seq.singleton x) y
        let foldBack = Seq.foldBack
        let initState = unit Seq.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the seq

    let traverseListA f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty
        let folder x state = prepend <!> (f x) <*> state
        foldBack folder m initState // right fold over the seq

    let traverseListM f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the option

    let inline sequenceSeqA m = traverseSeqA id m
    let inline sequenceSeqM m = traverseSeqM id m
    let inline sequenceListA m = traverseListA id m
    let inline sequenceListM m = traverseListM id m
