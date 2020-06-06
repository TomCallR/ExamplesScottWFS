type LinkedList<'a> =
    | Empty
    | Cons of head:'a * tail:LinkedList<'a>

module LinkedList = 

    let rec cata fEmpty fCons list :'r =
        let recurse = cata fEmpty fCons
        match list with
        | Empty -> fEmpty
        | Cons (element, tail) ->
            fCons element (recurse tail)

    let rec fold fCons acc list :'r=
        let recurse = fold fCons
        match list with
        | Empty -> acc
        | Cons (element, tail) ->
            let newacc = fCons acc element
            recurse newacc tail

    let rec foldWithEmpty fCons fEmpty acc list :'r=
        let recurse = foldWithEmpty fCons fEmpty 
        match list with
        | Empty -> 
            fEmpty acc 
        | Cons (element,list) -> 
            let newAcc = fCons acc element 
            recurse newAcc list

    let foldBack fCons list acc :'r=    // explications en fin de fichier
        let fEmpty' generator = 
            generator acc 
        let fCons' generator element = 
            let xfunc innerResult = 
                let newResult = fCons element innerResult 
                generator newResult
            xfunc
        let initialGenerator = id
        foldWithEmpty fCons' fEmpty' initialGenerator list 

// Alternative way to write foldBack
    let foldBack2 fCons list acc : 'r =
        let rec recurse l cont =
            match l with
            | Empty -> cont acc
            | Cons(element, tail) ->
                let newcont = (fun innerval -> cont (fCons element innerval))
                recurse tail newcont
        recurse list id

    let toList linkedList =
        let fCons head tail = head::tail
        foldBack fCons linkedList []

    let ofList list =
        let fCons head tail = Cons (head,tail)
        List.foldBack fCons list Empty

    let map f list =
        let fEmpty = Empty
        let fCons head tail = Cons(f head, tail)
        cata fEmpty fCons list

    let filter f list =
        let fCons head tail =
            match f head with
            | true -> Cons(head, tail)
            | false -> tail
        foldBack fCons list Empty

    let rev list =
        let fCons tail head =
            Cons(head, tail)
        fold fCons Empty list

// Tests
// open LinkedList
let linkedList = Cons (1, Cons (2, Cons(3, Empty)))
printfn "Liste : %A" linkedList
printfn "Somme (fold) : %i" (linkedList |> LinkedList.fold (+) 0)
printfn "Somme (foldBack) : %i" (LinkedList.foldBack (+) linkedList 0)
printfn "Somme (foldBack2) : %i" (LinkedList.foldBack2 (+) linkedList 0)

printfn "Conversion : %A => %A" linkedList (linkedList |> LinkedList.toList)      // Result => [1; 2; 3]
let list = [1;2;3]
printfn "Conversion : %A => %A" list (list |> LinkedList.ofList)           // Result => Cons (1,Cons (2,Cons (3,Empty)))
printfn "Carrés : %A" (linkedList |> LinkedList.map (fun x -> x * x))
printfn "Impairs : %A" (linkedList |> LinkedList.filter (fun x -> (x%2) = 1))
printfn "Inversés : %A" (linkedList |> LinkedList.rev)

// Explications pour Fold et FoldBack avec l'exemple de la sommation des éléments
// de la liste : Cons (1, Cons (2, Cons(3, Empty)))
// * Fold
//  ** list = 1::[2..Empty]     donc    Cons (1, [2..Empty])
//      donc    fCons id 1     -> newAcc
//      et      recurse newAcc [2..Empty]
//  récursion :
//  ** list = 2::[3..Empty]     donc    Cons (2, [3..Empty])
//      donc    fCons (fCons id 1) 2   -> newAcc
//      et      recurse newAcc [3..Empty]
//  récursion :
//  ** list = 3::Empty          donc    Cons (3, Empty)
//      donc    fCons (fCons (fCons id 1) 2) 3    -> newAcc
//      et      recurse newAcc Empty
//  récursion :
//  ** list = Empty             donc    Empty
//      donc    fEmpty (fCons ( fCons (fCons id 1) 2) 3)
//
// * FoldBack : développement de fEmpty (fCons ( fCons (fCons id 1) 2) 3)
//  ** fCons id 1       devient     xfunc innerRes = id (+ 1 innerRes)
//                      soit        f(x) = 1 + x
//  ** fCons (f(x) = 1 + x) 2   devient     xfunc innerRes = f(+ 2 innerRes)
//                              soit        f1(x1) = 1 + (2 + x1)
//  ** fCons (f1(x1) = 1 + (2 + x1)) 3  devient     xfunc innerRes = f1(+ 3 innerRes)
//                                      soit        f2(x2) = 1 + (2 + (3 + x2))
//  ** fEmpty (f2(x2) = 1 + (2 + (3 + x2)))     devient     xfunc innerRes = f2(0)
//                                      soit        f3(x3) = 1 + (2 + (3 + 0)) = 6
//