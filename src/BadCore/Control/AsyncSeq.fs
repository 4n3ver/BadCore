namespace BadCore.Control

type AsyncSeq<'a> = Async<'a> seq

module AsyncSeq =
    open BadCore.Extensions

    let unit (x: 'a): AsyncSeq<'a> = (Async.unit >> Seq.unit) x

    let bind (f: 'a -> AsyncSeq<'b>) (m: AsyncSeq<'a>): AsyncSeq<'b> =
        seq {
            for s in m do
                failwith "no clue!"
        }

    let map (f: 'a -> 'b) (m: AsyncSeq<'a>): AsyncSeq<'b> = (Async.map >> Seq.map) f m

    let apply (fList: AsyncSeq<'a -> 'b>) (xList: AsyncSeq<'a>): AsyncSeq<'b> =
        seq {
            for f in fList do
                for x in xList do
                    yield Async.apply f x
        }

    // Derived
    let (<!>) = map
    let (<*>) = apply
    let (=<<) = bind
    let (>>=) m f = f =<< m
    let (>=>) f1 f2 = bind f2 << f1
    let (<=<) f2 f1 = f1 >=> f2
    let lift1 = (<!>)
    let lift2 f m1 m2 = f <!> m1 <*> m2
    let lift3 f m1 m2 m3 = f <!> m1 <*> m2 <*> m3
    let (<*) x y = lift2 (fun l _ -> l) x y
    let ( *> ) x y = lift2 (fun _ r -> r) x y

    let traverseSeqA f m =
        let prepend x y = Seq.append (Seq.singleton x) y
        let foldBack = Seq.foldBack
        let initState = unit Seq.empty
        let folder x state = prepend <!> (f x) <*> state
        foldBack folder m initState // right fold over the option

    let traverseSeqM f m =
        let prepend x y = Seq.append (Seq.singleton x) y
        let foldBack = Seq.foldBack
        let initState = unit Seq.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the option

    let traverseListA f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty
        let folder x state = prepend <!> (f x) <*> state
        foldBack folder m initState // right fold over the option

    let traverseListM f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the option

    let sequenceSeqA m = traverseSeqA id m
    let sequenceSeqM m = traverseSeqM id m
    let sequenceListA m = traverseListA id m
    let sequenceListM m = traverseListM id m
