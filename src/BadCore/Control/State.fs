namespace BadCore.Control

type State<'state, 'a> = State of ('state -> 'a * 'state)

module State =
    let run (State mutate: State<'state, 'a>) (state: 'state) : 'a * 'state = mutate state

    let get : State<'state, 'state> = State(fun mutate -> mutate, mutate)

    let put (state: 'state) : State<'state, unit> = State(fun _ -> (), state)


    // Monad
    let unit (x: 'a) : State<'state, 'a> = State(fun state -> x, state)

    let bind (f: 'a -> State<'state, 'b>) (m: State<'state, 'a>) : State<'state, 'b> =
        let mutate state =
            let a, state = run m state
            run (f a) state

        State mutate


    // Functor
    let map (f: 'a -> 'b) (m: State<'state, 'a>) : State<'state, 'b> =
        let mutate state =
            let a, state = run m state
            f a, state

        State mutate


    // Applicative
    let apply (fm: State<'state, 'a -> 'b>) (m: State<'state, 'a>) : State<'state, 'b> =
        let mutate state =
            let f, state = run fm state
            let a, state = run m state
            f a, state

        State mutate


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

type State<'state, 'a> with
    static member inline (<*)(x, y) = State.op_LessMultiply x y
    static member inline ( *> )(x, y) = State.op_MultiplyGreater x y
    static member inline (<!>)(f, m) = State.op_LessBangGreater f m
    static member inline (<*>)(fm, m) = State.op_LessMultiplyGreater fm m
    static member inline (=<<)(f, m) = State.op_EqualsLessLess f m
    static member inline (>>=)(m, f) = State.op_GreaterGreaterEquals m f
    static member inline (>=>)(f1, f2) = State.op_GreaterEqualsGreater f1 f2
    static member inline (<=<)(f2, f1) = State.op_LessEqualsLess f2 f1
