namespace BadCore.Control

type AsyncResult<'v, 'e> = Async<Result<'v, 'e>>

module AsyncResult =
    open BadCore.Extensions

    let unit (x: 'v): AsyncResult<'v, 'e> = (Result.unit >> Async.unit) x

    let bind (f: 'v -> AsyncResult<'r, 'e>) (m: AsyncResult<'v, 'e>): AsyncResult<'r, 'e> =
        async {
            let! m = m

            match m with
            | Ok v -> return! f v
            | Error e -> return Error e
        }

    let map (f: 'v -> 'r) (m: AsyncResult<'v, 'e>): AsyncResult<'r, 'e> = (Result.map >> Async.map) f m

    let mapError (f: 'e -> 'r) (m: AsyncResult<'v, 'e>): AsyncResult<'v, 'r> = (Result.mapError >> Async.map) f m

    let apply (fm: AsyncResult<'v -> 'r, 'e>) (m: AsyncResult<'v, 'e>): AsyncResult<'r, 'e> =
        async {
            // The apply function runs the two parameters in parallel using a fork/join pattern.
            // If I had instead written let! fChild = ... followed by a let! xChild = ...
            // that would have been monadic and sequential, which is not what I wanted.
            let! fm = Async.StartChild fm
            let! m = Async.StartChild m
            let! f = fm
            let! x = m
            return Result.apply f x
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

[<AutoOpen>]
module AsyncResultComputationExpression =
    type AsyncResultBuilder() =
        member __.Return(x) = AsyncResult.unit x
        member __.Bind(x, f) = AsyncResult.bind f x

        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return()

        member __.Delay(f) = f
        member __.Run(f) = f ()

        member this.While(guard, body) =
            if not (guard ()) then
                this.Zero()
            else
                this.Bind(body (), (fun () -> this.While(guard, body)))

        member this.TryWith(body, handler) =
            try
                this.ReturnFrom(body ())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try
                this.ReturnFrom(body ())
            finally
                compensation ()

        member this.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable

            this.TryFinally(
                body',
                (fun () ->
                    match disposable with
                    | null -> ()
                    | disp -> disp.Dispose())
            )

        member this.For(sequence: seq<_>, body) =
            this.Using(
                sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
            )

        member this.Combine(a, b) = this.Bind(a, (fun () -> b ()))

    let asyncResult = AsyncResultBuilder()
