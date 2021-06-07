namespace BadCore.Control

type AsyncSeq<'a> = Async<'a> seq

module AsyncSeq =
    open BadCore.Extensions

    let unit (x: 'a) : AsyncSeq<'a> = (Async.unit >> Seq.unit) x

    let bind (f: 'a -> AsyncSeq<'b>) (m: AsyncSeq<'a>) : AsyncSeq<'b> =
        let rec asyncFold state sequence =
            async {
                if Seq.isEmpty sequence then
                    return state |> Seq.collect id |> Seq.rev
                else
                    let! head = Seq.head sequence
                    let state = (f head) :: state
                    return! asyncFold state (Seq.tail sequence)
            }

        asyncFold [] m |> Async.RunSynchronously

    let map (f: 'a -> 'b) (m: AsyncSeq<'a>) : AsyncSeq<'b> = (Async.map >> Seq.map) f m

    let apply (fList: AsyncSeq<'a -> 'b>) (xList: AsyncSeq<'a>) : AsyncSeq<'b> =
        seq {
            for f in fList do
                for x in xList do
                    yield Async.apply f x
        }

    let rec tryFind (predicate: 'a -> bool) (m: AsyncSeq<'a>) : Async<'a option> =
        async {
            if Seq.isEmpty m then
                return None
            else
                let! x = Seq.head m

                if predicate x then
                    return Some x
                else
                    return! tryFind predicate (Seq.tail m)
        }

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
    let inline ( *> ) x y = lift2 (fun _ r -> r) x y

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

    let inline sequenceSeqA m = traverseSeqA id m
    let inline sequenceSeqM m = traverseSeqM id m
    let inline sequenceListA m = traverseListA id m
    let inline sequenceListM m = traverseListM id m
