type Book = {title: string; price: decimal}

type ChocolateType = Dark | Milk | SeventyPercent
type Chocolate = {chocType: ChocolateType ; price: decimal}

type WrappingPaperStyle = 
    | HappyBirthday
    | HappyHolidays
    | SolidColor

type Gift =
    | Book of Book
    | Chocolate of Chocolate 
    | Wrapped of Gift * WrappingPaperStyle
    | Boxed of Gift 
    | WithACard of Gift * message:string

let rec cataGift fBook fChocolate fWrapped fBox fCard gift :'r =
    let recurse = cataGift fBook fChocolate fWrapped fBox fCard
    match gift with 
    | Book book -> 
        fBook book
    | Chocolate choc -> 
        fChocolate choc
    | Wrapped (gift,style) -> 
        fWrapped (recurse gift,style)
    | Boxed gift -> 
        fBox (recurse gift)
    | WithACard (gift,message) -> 
        fCard (recurse gift,message)

let totalCostUsingCata gift =
    let fBook (book:Book) = 
        book.price
    let fChocolate (choc:Chocolate) = 
        choc.price
    let fWrapped  (innerCost,style) = 
        innerCost + 0.5m
    let fBox innerCost = 
        innerCost + 1.0m
    let fCard (innerCost,message) = 
        innerCost + 2.0m
    // call the catamorphism
    cataGift fBook fChocolate fWrapped fBox fCard gift
    
// Tests
// A Book
let wolfHall = {title="Wolf Hall"; price=20m}
// A Chocolate
let yummyChoc = {chocType=SeventyPercent; price=5m}
// A Gift
let birthdayPresent = WithACard (Wrapped (Book wolfHall, HappyBirthday), "Happy Birthday")
// A Gift
let christmasPresent = Wrapped (Boxed (Chocolate yummyChoc), HappyHolidays)

let deeplyNestedBox depth =
    let rec loop depth boxSoFar =
        match depth with
        | 0 -> boxSoFar 
        | n -> loop (n-1) (Boxed boxSoFar)
    loop depth (Book wolfHall)

let rec totalCostUsingAcc costSoFar gift =
    match gift with 
    | Book book -> 
        costSoFar + book.price  // final result
    | Chocolate choc -> 
        costSoFar + choc.price  // final result
    | Wrapped (innerGift,style) -> 
        let newCostSoFar = costSoFar + 0.5m
        totalCostUsingAcc newCostSoFar innerGift 
    | Boxed innerGift -> 
        let newCostSoFar = costSoFar + 1.0m
        totalCostUsingAcc newCostSoFar innerGift 
    | WithACard (innerGift,message) -> 
        let newCostSoFar = costSoFar + 2.0m
        totalCostUsingAcc newCostSoFar innerGift 

// Catamorphism with fold
let rec foldGift fBook fChocolate fWrapped fBox fCard acc gift :'r =
    let recurse = foldGift fBook fChocolate fWrapped fBox fCard
    match gift with
    | Book book ->
        let finalAcc = fBook acc book
        finalAcc
    | Chocolate choc ->
        let finalAcc = fChocolate acc choc
        finalAcc
    | Wrapped (innerGift, style) ->
        let newAcc =  fWrapped acc style
        recurse newAcc innerGift
    | Boxed innerGift ->
        let newAcc = fBox acc
        recurse newAcc innerGift
    | WithACard (innerGift, message) ->
        let newAcc = fCard acc message
        recurse newAcc innerGift

// Total cost with fold
let totalCostUsingFold gift =
    let fBook costSoFar (book:Book) = costSoFar + book.price
    let fChocolate costSoFar (choc:Chocolate) = costSoFar + choc.price
    let fWrapped costSoFar (style:WrappingPaperStyle) = costSoFar + 0.5m
    let fBox costSoFar = costSoFar + 1.0m
    let fCard costSoFar (message:string) = costSoFar + 2.0m
    foldGift fBook fChocolate fWrapped fBox fCard 0.0m gift

// Description with fold
// Return type is a function generating a string from a gift
let descriptionUsingFoldWithGenerator gift =
    let fBook descrGenerator (book:Book) =
        descrGenerator (sprintf "%s" book.title)
    let fChocolate descrGenerator (choc:Chocolate) =
        descrGenerator (sprintf "%A chocolate" choc.chocType)
    let fWrapped descrGenerator style =
        fun innerText ->
            let newInnerText = sprintf "%s wrapped in %A paper" innerText style
            descrGenerator newInnerText
    let fBox descrGenerator =
        fun innerText ->
            let newInnerText = sprintf "%s in a box" innerText
            descrGenerator newInnerText
    let fCard descrGenerator message =
        fun innerText ->
            let newInnerText = sprintf "%s with a card saying '%s'" innerText message
            descrGenerator newInnerText
    foldGift fBook fChocolate fWrapped fBox fCard id gift

// Foldback
let rec foldbackGift fBook fChocolate fWrapped fBox fCard generator gift :'r =
    let recurse = foldbackGift fBook fChocolate fWrapped fBox fCard 
    match gift with 
    | Book book ->
        generator (fBook book)
    | Chocolate choc ->
        generator (fChocolate choc)
    | Wrapped (innerGift, style) ->
        let newGenerator innerVal =
            generator (fWrapped innerVal style)
        recurse newGenerator innerGift
    | Boxed innerGift ->
        let newGenerator innerVal =
            generator (fBox innerVal)
        recurse newGenerator innerGift
    | WithACard (innerGift, message) ->
        let newGenerator innerVal =
            generator (fCard innerVal message)
        recurse newGenerator innerGift

let descriptionUsingFoldback gift =
    let fBook (book:Book) =
        sprintf "'%s'" book.title
    let fChocolate (choc:Chocolate) =
        sprintf "%A chocolate" choc.chocType
    let fWrapped innerDesc style =
        sprintf "%s wrapped in %A paper" innerDesc style
    let fBox innerDesc =
        sprintf "%s in a box" innerDesc
    let fCard innerDesc message =
        sprintf "%s with a card saying '%s'" innerDesc message
    foldbackGift fBook fChocolate fWrapped fBox fCard id gift    

// printfn "%f" (deeplyNestedBox 100000 |> totalCostUsingCata) // with depth 100000 => stack overflow
// printfn "%f" (deeplyNestedBox 100000 |> totalCostUsingAcc 0.0m)

printfn "%s" (birthdayPresent |> descriptionUsingFoldWithGenerator) // CORRECT "'Wolf Hall' wrapped in HappyBirthday paper with a card saying 'Happy Birthday'"
printfn "%s" (christmasPresent |> descriptionUsingFoldWithGenerator) // CORRECT "SeventyPercent chocolate in a box wrapped in HappyHolidays paper"
printfn "%s" (birthdayPresent |> descriptionUsingFoldback)
printfn "%s" (christmasPresent |> descriptionUsingFoldback)