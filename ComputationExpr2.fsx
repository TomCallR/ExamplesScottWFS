// wrappers

type StringIntBuilder() =

    member this.Bind(m: string, f) =
        let b, i = System.Int32.TryParse(m)
        match b, i with
        | false, _ -> "error"
        | true, i -> f i

    member this.Return(x) = sprintf "%i" x

let stringint = StringIntBuilder()

let good1 =
    stringint {
        let! i = "42"
        let! j = "43"
        return i + j
    }

printfn "good1=%s" good1

let bad1 =
    stringint {
        let! i = "42"
        let! j = "xxx"
        return i + j
    }

printfn "bad1=%s" bad1

let g1 = "99"

let g2 =
    stringint {
        let! i = g1
        return i
    }

printfn "g1=%s g2=%s" g1 g2

let b1 = "xxx"

let b2 =
    stringint {
        let! i = b1
        return i
    }

printfn "b1=%s b2=%s" b1 b2

// List wrapper
type ListWorkflowBuilder() =
    member this.Bind(l, f) = l |> List.collect f
    member this.Return(x) = [ x ]
    member this.For(l, f) = this.Bind(l, f)

let listWorkflow = ListWorkflowBuilder()

let added =
    listWorkflow {
        let! x = [ 1; 2; 3 ]
        let! y = [ 10; 11; 12 ]
        return x + y
    }

printfn "added=%A" added

let multiplied =
    listWorkflow {
        let! x = [ 1; 2; 3 ]
        let! y = [ 10; 11; 12 ]
        return x * y
    }

printfn "added=%A" multiplied

let multiplied2 =
    listWorkflow {
        for i in [ 1; 2; 3 ] do
            for j in [ 10; 11; 12 ] do
                return i * j
    }

printfn "added=%A" multiplied2

// Trace Builder
type TraceBuilder() =
    member this.Bind(m, f) =
        match m with
        | None -> printfn "Binding with None. Exiting"
        | Some a -> printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) =
        printfn "Returning an unwrapped %A as an option" x
        Some x

    member this.ReturnFrom(m) =
        printfn "Returning an option (%A) directly"
        |> ignore
        m

    member this.Zero() =
        printfn "Zero"
        None

    member this.Yield(x) =
        printfn "Yield an unwrapped %A as an option" x
        Some x

    member this.YieldFrom(m) =
        printfn "Yield an option (%A) directly" |> ignore
        m

    member this.Combine(a, b) =
        match a, b with
        | Some a', Some b' ->
            printfn "combining %A and %A" a' b'
            Some(a' + b')
        | Some a', None ->
            printfn "combining %A with None" a'
            Some a'
        | None, Some b' ->
            printfn "combining None with %A" b'
            Some b'
        | None, None ->
            printfn "combining None with None"
            None

    member this.Delay(f) =
        printfn "Delay"
        f ()

let trace = TraceBuilder()

trace {
    printfn "Part 1: about to return 1"
    return 1
    printfn "Part 2: after return has happened"
}
|> printfn "Result for Part1 without Part2: %A"
// trace { return 1 } |> printfn "Result 1: %A"

// trace { return! Some 2 } |> printfn "Result 2: %A"

// trace {
//     let! x = Some 1
//     let! y = Some 2
//     return x + y
// }
// |> printfn "Result 3: %A"

// trace {
//     let! x = None
//     let! y = Some 1
//     return x + y
// }
// |> printfn "Result 4: %A"

trace {
    do! Some(printfn "...expression that returns unit")
    do! Some(printfn "...another expression that returns unit")
    let! x = Some(1)
    return x
}
|> printfn "Result from do: %A"

// trace { printfn "hello world" }
// |> printfn "Result for simple expression: %A"

// trace { if false then return 1 }
// |> printfn "Result for if without else: %A"

trace {
    yield 1
    let! x = None
    yield 2
}
|> printfn "Result for yield then yield: %A"

/// List Builder
type ListBuilder() =
    member this.Bind(m, f) = m |> List.collect f

    member this.Return(x) =
        printfn "Return an unwrapped %A as a list" x
        [ x ]

    member this.ReturnFrom(m) = m

    member this.Yield(x) =
        printfn "Yield an unwrapped %A as a list" x
        [ x ]

    member this.YieldFrom(m) = m

    member this.Zero =
        printfn "Zero"
        []

    member this.For(m, f) =
        printfn "For %A" m
        this.Bind(m, f)

    member this.Combine(a, b) =
        printfn "combining %A and %A" a b
        List.concat [ a; b ]

    member this.Delay(f) =
        printfn "Delay"
        f ()

let listbuilder = ListBuilder()

// listbuilder {
//     let! x = [ 1 .. 3 ]
//     let! y = [ 10; 20; 30 ]
//     return x + y
// }
// |> printfn "Result: %A"

// listbuilder {
//     for x in [ 1 .. 3 ] do
//         for y in [ 10; 20; 30 ] do
//             return x + y
// }
// |> printfn "Result: %A"

// listbuilder {
//     yield 1
//     yield 2
// }
// |> printfn "Result for yield then yield: %A"

// listbuilder {
//     yield 1
//     yield! [ 2; 3 ]
// }
// |> printfn "Result for yield then yield! : %A"

listbuilder {
    for i in [ "red"; "blue" ] do
        yield i
        for j in [ "hat"; "tie" ] do
            yield! [ i + " " + j; "-" ]
}
|> printfn "Result for for..in..do : %A"

// Overloading
type SuccessOrError<'a, 'b> =
    | Success of 'a
    | Error of 'b

type SuccessOrErrorBuilder() =

    member this.Bind(m, f) =
        match m with
        | Success s ->
            try
                f s
            with e -> Error e.Message
        | Error _ -> m

    member this.Return(x) = Success x

let successOrError = SuccessOrErrorBuilder()

successOrError { return 42 }
|> printfn "Result for success: %A"

successOrError {
    let! x = Success 1
    return x / 0
}
|> printfn "Result for error: %A"

// Overloading2
type ListBuilder2() =
    member this.Bind(m, f) = m |> List.collect f

    member this.Yield(x) =
        printfn "Yield an unwrapped %A as a list" x
        [ x ]

    member this.For(m, f) =
        printfn "For %A" m
        this.Bind(m, f)

    member this.For(m: _ seq, f) =
        printfn "For %A using seq" m
        let m2 = List.ofSeq m
        this.Bind(m2, f)

let listbuilder2 = ListBuilder2()

listbuilder2 {
    let list = [ 1 .. 10 ]
    for i in list do
        yield i
}
|> printfn "Result for list: %A"

listbuilder2 {
    let s = seq { 1 .. 10 }
    for i in s do
        yield i
}
|> printfn "Result for seq : %A"

// Lazyness
type Maybe<'a> = Maybe of (unit -> 'a option)

type MaybeBuilder() =

    member this.Bind(m, f) = Option.bind f m

    member this.Return(x) = Some x

    member this.ReturnFrom(Maybe f) = f ()

    member this.Zero() = None

    member this.Combine(a, b) =
        match a with
        | Some _' -> a // if a is good, skip b
        | None -> b () // if a is bad, run b

    member this.Delay(f) = f

    member this.Run(f) = Maybe f

let maybe = MaybeBuilder()

let run (Maybe f) = f ()

// maybe {
//     printfn "Part 1: about to return 1"
//     return 1
//     printfn "Part 2: after return has happened"
// }
// |> printfn "Result for Part1 but not Part2: %A"

// maybe {
//     printfn "Part 1: about to return None"
//     return! None
//     printfn "Part 2: after None, keep going"
// }
// |> printfn "Result for Part1 and then Part2: %A"

// let childWorkflow = maybe { printfn "Child workflow" }

// maybe {
//     printfn "Part 1: about to return 1"
//     return 1
//     return! childWorkflow
// }
// |> printfn "Result for Part1 but not childWorkflow: %A"

let m1 =
    maybe {
        printfn "Part 1: about to return 1"
        return 1
        printfn "Part 2: after return has happened"
    }

run m1
|> printfn "Result for Part1 but not Part2: %A"
