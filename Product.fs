module Product

type Product =
    | Bought of BoughtProduct
    | Made of MadeProduct

and BoughtProduct =
    { Name: string
      Weight: int
      Vendor: string option }

and MadeProduct =
    { Name: string
      Weight: int
      Components: Component list }

and Component = { Qty: int; Product: Product }

type VendorScore = { Vendor: string; Score: int }

let rec cataProduct fBought fMade product: 'r =
    let recurse = cataProduct fBought fMade
    let convertComponentToTuple comp = (comp.Qty, recurse comp.Product)
    match product with
    | Bought bought -> fBought bought
    | Made made ->
        let componentTuples =
            made.Components
            |> List.map convertComponentToTuple

        fMade (made.Name, made.Weight, componentTuples)

//
let productWeight product =
    let fBought (bought: BoughtProduct) = bought.Weight

    let fMade (name, weight, components) =
        let totalCompWeights =
            components |> List.sumBy (fun (q, w) -> q * w)

        weight + totalCompWeights

    cataProduct fBought fMade product

//
let vendor vs = vs.Vendor
let score vs = vs.Score

//
let mostUsedVendor product =
    let fBought (bought: BoughtProduct) =
        bought.Vendor
        |> Option.map (fun vendor -> { Vendor = vendor; Score = 1 })
        |> Option.toList

    let totalScore (vendor, vslist) =
        let scoreSum = vslist |> List.sumBy score
        { Vendor = vendor; Score = scoreSum }

    let fMade (name, weight, subresults) =
        subresults
        |> List.collect snd
        |> List.groupBy vendor
        |> List.map totalScore

    cataProduct fBought fMade product
    |> List.sortByDescending score
    |> List.tryHead

// Test
let label =
    Bought
        { Name = "label"
          Weight = 1
          Vendor = Some "ACME" }

let bottle =
    Bought
        { Name = "bottle"
          Weight = 2
          Vendor = Some "ACME" }

let formulation =
    Bought
        { Name = "formulation"
          Weight = 3
          Vendor = None }

let shampoo =
    Made
        { Name = "shampoo"
          Weight = 10
          Components =
              [ { Qty = 1; Product = formulation }
                { Qty = 1; Product = bottle }
                { Qty = 2; Product = label } ] }

let twoPack =
    Made
        { Name = "twoPack"
          Weight = 5
          Components = [ { Qty = 2; Product = shampoo } ] }

printfn "%i" (label |> productWeight) // 1
printfn "%i" (shampoo |> productWeight) // 17 = 10 + (2x1 + 1x2 + 1x3)
printfn "%i" (twoPack |> productWeight) // 39 = 5 + (2x17)

printfn "%A" (label |> mostUsedVendor) // Some {vendor = "ACME"; score = 1}
printfn "%A" (formulation |> mostUsedVendor) // None
printfn "%A" (shampoo |> mostUsedVendor) // Some {vendor = "ACME"; score = 2}
printfn "%A" (twoPack |> mostUsedVendor) // Some {vendor = "ACME"; score = 2}
