namespace BadCore.Extensions

module Option =
    let unit (x: 'a): 'a option = Some x

    let bind (f: 'a -> 'b option) (m: 'a option): 'b option = Option.bind f m

    let map (f: 'a -> 'b) (m: 'a option): 'b option = Option.map f m

    let apply (fm: ('a -> 'b) option) (m: 'a option): 'b option =
        match fm, m with
        | Some f, Some x -> Some(f x)
        | _ -> None

    let getOrError (e: Lazy<'e>) (m: 'v option): Result<'v, 'e> =
        match m with
        | Some v -> Ok v
        | _ -> Error(e.Force())

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
