namespace BadCore.Control

open BadCore

type Reader<'env, 'a> = Reader of ('env -> 'a)

module Reader =
    let run (Reader read: Reader<'env, 'a>) (env: 'env) : 'a = read env


    // Monad
    let unit (x: 'a) : Reader<'env, 'a> = Reader(Function.constant x)

    let bind (f: 'a -> Reader<'env, 'b>) (m: Reader<'env, 'a>) : Reader<'env, 'b> =
        let read env =
            let a = run m env
            run (f a) env

        Reader read


    // Functor
    let map (f: 'a -> 'b) (m: Reader<'env, 'a>) : Reader<'env, 'b> = Reader(run m >> f)


    // Applicative
    let apply (fm: Reader<'env, 'a -> 'b>) (m: Reader<'env, 'a>) : Reader<'env, 'b> =
        Reader(fun env -> (run fm env) (run m env))


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

type Reader<'env, 'a> with
    static member inline (<*)(x, y) = Reader.op_LessMultiply x y
    static member inline ( *> )(x, y) = Reader.op_MultiplyGreater x y
    static member inline (<!>)(f, m) = Reader.op_LessBangGreater f m
    static member inline (<*>)(fm, m) = Reader.op_LessMultiplyGreater fm m
    static member inline (=<<)(f, m) = Reader.op_EqualsLessLess f m
    static member inline (>>=)(m, f) = Reader.op_GreaterGreaterEquals m f
    static member inline (>=>)(f1, f2) = Reader.op_GreaterEqualsGreater f1 f2
    static member inline (<=<)(f2, f1) = Reader.op_LessEqualsLess f2 f1
