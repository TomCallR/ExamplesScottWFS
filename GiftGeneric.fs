type Book = {Title: string; Price: decimal}

type ChocolateType = Dark | Milk | SeventyPercent
type Chocolate = {ChocType: ChocolateType ; Price: decimal}

type WrappingPaperStyle = 
    | HappyBirthday
    | HappyHolidays
    | SolidColor

type GiftContents =
    | Book of Book
    | Chocolate of Chocolate

type GiftDecoration =
    | Boxed
    | Wrapped of WrappingPaperStyle
    | WithACard of string

// type Gift =
//     | Contents of GiftContents
//     | Decoration of Gift * GiftDecoration

type Container<'ContentData, 'DecorationData> =
    | Contents of 'ContentData
    | Decoration of 'DecorationData * Container<'ContentData, 'DecorationData>

type Gift = Container<GiftContents,GiftDecoration>

module Container =

    let rec cata fContents fDecoration (container:Container<'ContentData, 'DecorationData>) : 'r=
        let recurse = cata fContents fDecoration
        match container with
        | Contents content ->
            fContents content
        | Decoration (decoration, container) ->
            fDecoration decoration (recurse container)

    let rec fold fContents fDecoration acc (container:Container<'ContentData, 'DecorationData>):'r = 
        let recurse = fold fContents fDecoration
        match container with
        | Contents (contentData:'ContentData) ->
            fContents acc contentData
        | Decoration (decorationData, subContainer) ->
            let newAcc = fDecoration acc decorationData
            recurse newAcc subContainer        

    let rec foldBack fContents fDecoration (container:Container<'ContentData, 'DecorationData>) :'r =
        let fContents' generator (contentData:'ContentData) =
            generator (fContents contentData)
        let fDecoration' generator decorationData =
            let newGenerator innerVal =
                let newInnerValue = fDecoration decorationData innerVal
                generator newInnerValue
            newGenerator
        fold fContents' fDecoration' id container

    let fromBook book = 
        Contents (Book book)

    let fromChoc choc = 
        Contents (Chocolate choc)

    let wrapInPaper paperStyle innerGift = 
        let container = Wrapped paperStyle 
        Decoration (container, innerGift)

    let putInBox innerGift = 
        let container = Boxed
        Decoration (container, innerGift)

    let withCard message innerGift = 
        let container = WithACard message
        Decoration (container, innerGift)

    let totalCost gift =
        let fContents acc contentData =
            match contentData with
            | Book b -> acc + b.Price
            | Chocolate c -> acc + c.Price
        let fDecoration acc decorationData =
            match decorationData with
            | Boxed -> acc + 1.0m
            | Wrapped _ -> acc + 0.5m
            | WithACard _ -> acc + 2.0m
        fold fContents fDecoration 0.m gift

    let description gift =
        let fContents contentData =
            match contentData with
            | Book b -> sprintf "'%s'" b.Title
            | Chocolate c -> sprintf "%A chocolate" c.ChocType
        let fDecoration decorationData innerText =
            match decorationData with
            | Boxed -> sprintf "%s in a box" innerText
            | Wrapped style -> sprintf "%s wrapped in %A paper" innerText style
            | WithACard m -> sprintf "%s with a card saying '%s'" innerText m
        foldBack fContents fDecoration gift

// Tests
open Container

let wolfHall = {Title="Wolf Hall"; Price=20m}
let yummyChoc = {ChocType=SeventyPercent; Price=5m}

let birthdayPresent = 
    wolfHall 
    |> fromBook
    |> wrapInPaper HappyBirthday
    |> withCard "Happy Birthday"
 
let christmasPresent = 
    yummyChoc
    |> fromChoc
    |> putInBox
    |> wrapInPaper HappyHolidays

printfn "Birthday cost : %f" (birthdayPresent |> totalCost)     // 22.5m
printfn "Christmas cost : %f" (christmasPresent |> totalCost)    // 6.5m
printfn "%s" (birthdayPresent |> description)
// CORRECT "'Wolf Hall' wrapped in HappyBirthday paper with a card saying 'Happy Birthday'"
printfn "%s" (christmasPresent |> description)
// CORRECT "SeventyPercent chocolate in a box wrapped in HappyHolidays paper"
