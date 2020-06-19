let pchar0 charToMatch str =
    if (String.length str) = 0 then Error "No more input"
    else
        let first = str.[0]
        if first = charToMatch
        then
            let remaining = str.[1..]
            Ok (charToMatch, remaining)
        else
            let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
            Error msg

// pchar0 'A' "JBC" |> (printfn "%A")
let parseA0 = pchar0 'A'
parseA0 "TBD" |> printfn "%A"

type Parser<'T> = Parser of (string -> Result<'T * string, string>)

let pchar charToMatch =
    let innerFn str =
        if String.length str = 0 then
            Error "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Ok (charToMatch, remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                Error msg
    Parser innerFn

let run parser input =
    let (Parser innerFn) =parser
    innerFn input


let parseA = pchar 'A'
run parseA "ABC" |> (printfn "%A")