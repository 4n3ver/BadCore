namespace BadCore.Extensions

module Async =
    // Monad
    let unit (x: 'a) : Async<'a> = async { return x }

    let bind (f: 'a -> Async<'b>) (m: Async<'a>) : Async<'b> =
        async {
            let! a = m
            return! f a
        }


    // Functor
    let map (f: 'a -> 'b) (m: Async<'a>) : Async<'b> =
        async {
            let! a = m
            return f a
        }


    // Applicative
    let apply (fm: Async<'a -> 'b>) (m: Async<'a>) : Async<'b> =
        async {
            // The apply function runs the two parameters in parallel using a fork/join pattern.
            // If I had instead written let! fChild = ... followed by a let! xChild = ...
            // that would have been monadic and sequential, which is not what I wanted.
            let! fm = Async.StartChild fm
            let! m = Async.StartChild m
            let! f = fm
            let! x = m
            return f x
        }


    let sleep (millis: int) : Async<unit> = Async.Sleep millis


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
        foldBack folder m initState // right fold over the async

    let traverseSeqM f m =
        let prepend x y = Seq.append (Seq.singleton x) y
        let foldBack = Seq.foldBack
        let initState = unit Seq.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the async

    let traverseListA f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty
        let folder x state = prepend <!> (f x) <*> state
        foldBack folder m initState // right fold over the async

    let traverseListM f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the async

    let inline sequenceSeqA m = traverseSeqA id m
    let inline sequenceSeqM m = traverseSeqM id m
    let inline sequenceListA m = traverseListA id m
    let inline sequenceListM m = traverseListM id m
