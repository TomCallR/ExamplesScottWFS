let log p = printfn "Expression is %A" p

let loggedWorkFlow0 =
    let x = 42
    log x
    let y = 43
    log y
    let z = x + y
    log z
    z

type LoggingBuilder() =
    let log p = printfn "Expression is %A" p

    member this.Bind(x, f) =
        log x
        f x

    member this.Return(x) = x

let logger = LoggingBuilder()

let loggedWorkFlow =
    logger {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
    }

// Safe division
let divideBy bottom top =
    if bottom = 0 then None else Some(top / bottom)

let divideByWorkFlow0 init x y z =
    let a = init |> divideBy x
    match a with
    | None -> None
    | Some a' ->
        let b = a' |> divideBy y
        match b with
        | None -> None
        | Some b' ->
            let c = b' |> divideBy z
            match c with
            | None -> None
            | Some c' -> Some c'

let good1 = divideByWorkFlow0 12 3 2 1
let bad1 = divideByWorkFlow0 12 3 0 1

type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some x' -> f x'

    member this.Return(x) = Some x

let maybe = MaybeBuilder()

let divideByWorkFlow init x y z =
    maybe {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
    }

// Or else tests
let map1 =
    [ ("1", "One"); ("2", "Two") ] |> Map.ofList

let map2 =
    [ ("A", "Alice"); ("B", "Bob") ] |> Map.ofList

let map3 =
    [ ("CA", "California")
      ("NY", "New York") ]
    |> Map.ofList

let multiLookup0 key =
    match map1.TryFind key with
    | Some result1 -> Some result1
    | None ->
        match map2.TryFind key with
        | Some result2 -> Some result2
        | None ->
            match map3.TryFind key with
            | Some result3 -> Some result3
            | None -> None

multiLookup0 "A" |> printfn "Result for A is %A"
multiLookup0 "CA" |> printfn "Result for CA is %A"
multiLookup0 "X" |> printfn "Result for X is %A"

type OrElseBuilder() =
    member this.ReturnFrom(x) = x

    member this.Combine(a, b) =
        match a with
        | Some _ -> a
        | None -> b

    member this.Delay(f) = f ()

let orElse = OrElseBuilder()

let multiLookup key =
    orElse {
        return! map1.TryFind key
        return! map2.TryFind key
        return! map3.TryFind key
    }
/// Logger

// Asynchronous calls
open System.Net

let req1 =
    HttpWebRequest.Create("http://fsharp.org")

let req2 =
    HttpWebRequest.Create("http://google.com")

let req3 = HttpWebRequest.Create("http://bing.com")

// req1.BeginGetResponse
//     ((fun r1 ->
//         use resp1 = req1.EndGetResponse(r1)
//         printfn "Downloaded %O" resp1.ResponseUri

//         req2.BeginGetResponse
//             ((fun r2 ->
//                 use resp2 = req2.EndGetResponse(r2)
//                 printfn "Downloaded %O" resp2.ResponseUri

//                 req3.BeginGetResponse
//                     ((fun r3 ->
//                         use resp3 = req3.EndGetResponse(r3)
//                         printfn "Downloaded %O" resp3.ResponseUri

//                      ),
//                      null)
//                 |> ignore),
//              null)
//         |> ignore),
//      null)
// |> ignore
async {
    use! resp1 = req1.AsyncGetResponse()
    printfn "Downloaded %O" resp1.ResponseUri
    use! resp2 = req2.AsyncGetResponse()
    printfn "Downloaded %O" resp2.ResponseUri
    use! resp3 = req3.AsyncGetResponse()
    printfn "Downloaded %O" resp3.ResponseUri
}
|> Async.RunSynchronously


//This simple workflow just sleeps for 2 seconds.
open System

let sleepWorkflow =
    async {
        printfn "Starting sleep workflow at %O" DateTime.Now.TimeOfDay
        do! Async.Sleep 14000
        printfn "Finished sleep workflow at %O" DateTime.Now.TimeOfDay
    }

let nestedWorkflow =
    async {
        printfn "Starting parent"
        let! childWorkflow = Async.StartChild sleepWorkflow
        do! Async.Sleep 3000
        printfn "Doing something useful while waiting "
        let! result = childWorkflow
        printfn "Finished parent"
    }

Async.RunSynchronously nestedWorkflow

// Divide using custom continuation
// let divideBy bottom top =
//     if bottom = 0 then None else Some(top / bottom)

// let pipeInto (someExpression, lambda) =
//     match someExpression with
//     | None -> None
//     | Some x -> x |> lambda

// let return' c = Some c

// let divideByWorkflow x y w z =
//     pipeInto (x |> divideBy y, (fun a -> pipeInto (a |> divideBy w, (fun b -> pipeInto (b |> divideBy z, return')))))

// let good2 = divideByWorkflow 12 3 2 1
// let bad2 = divideByWorkflow 12 3 0 1

// Exercice : parsing a string
let strToInt (str: string) =
    try
        Some(int str)
    with _ -> None

type YourWorkFlowBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some value -> f value

    member this.Return(x) = Some x

let yourWorkflow = YourWorkFlowBuilder()

let stringAddWorkflow x y z =
    yourWorkflow {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
    }

// let good3 = stringAddWorkflow "12" "3" "2"
// printfn "%A" good3
// let bad3 = stringAddWorkflow "12" "xyz" "2"
// printfn "%A" bad3

let strAdd str i =
    yourWorkflow {
        let! a = strToInt str
        // let! ii = i
        return a + i
    }

let (>>=) m f =
    match m with
    | None -> None
    | Some i -> f i


let good4 =
    (strToInt "1") >>= (strAdd "2") >>= strAdd "3"

let bad4 =
    strToInt "1" >>= strAdd "xyz" >>= strAdd "3"
