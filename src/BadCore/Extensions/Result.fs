namespace BadCore.Extensions

module Result =
    // Monad
    let unit (x: 'v) : Result<'v, 'e> = Ok x

    let bind (f: 'v -> Result<'r, 'e>) (m: Result<'v, 'e>) : Result<'r, 'e> =
        match m with
        | Ok v -> f v
        | Error e -> Error e


    // Functor
    let map (f: 'a -> 'b) (m: Result<'a, 'e>) : Result<'b, 'e> = Result.map f m


    // Applicative
    let apply (fm: Result<'v -> 'r, 'e>) (m: Result<'v, 'e>) : Result<'r, 'e> =
        match fm, m with
        | Ok f, Ok v -> Ok(f v)
        | Error e, _
        | _, Error e -> Error e


    let isOk (m: Result<'a, 'e>) : bool =
        match m with
        | Ok _ -> true
        | Error _ -> false

    let isError (m: Result<'a, 'e>) : bool =
        match m with
        | Ok _ -> false
        | Error _ -> true

    let ignoreError (m: Result<'a, 'e>) : 'a option =
        match m with
        | Ok a -> Some a
        | Error _ -> None

    let get (m: Result<'a, 'e>) : 'a =
        match m with
        | Ok a -> a
        | Error e -> invalidArg "result" $"Cannot obtain value due to error: {e}"


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
        foldBack folder m initState // right fold over the result

    let traverseSeqM f m =
        let prepend x y = Seq.append (Seq.singleton x) y
        let foldBack = Seq.foldBack
        let initState = unit Seq.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the result

    let traverseListA f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty
        let folder x state = prepend <!> (f x) <*> state
        foldBack folder m initState // right fold over the result

    let traverseListM f m =
        let prepend x m = x :: m
        let foldBack = List.foldBack
        let initState = unit List.empty

        let folder x state =
            f x
            >>= (fun h -> state >>= (fun t -> unit (prepend h t)))

        foldBack folder m initState // right fold over the result

    let inline sequenceSeqA m = traverseSeqA id m
    let inline sequenceSeqM m = traverseSeqM id m
    let inline sequenceListA m = traverseListA id m
    let inline sequenceListM m = traverseListM id m
