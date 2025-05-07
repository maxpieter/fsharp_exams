// I hereby declare that I myself have created this exam hand–in in its entirety without help from
// anybody else.

type TrashItem<'a> =
    | Paper of string
    | Glass of string
    | Other of 'a

let item1 = Other("Shirt", "Clothes")
let item2 = Paper "Newspaper"
let item3 = Other("Sneakers", "Shoe")
let item4 = Glass "Wine glass"

// item1 has type TrashItem<string * string>
// item2 is of type TrashItem<'a> since no generic cases are concretly implemented

// TrashItem is polymorphic as it allows you to use any type for its concrete implementation
// through Other<'a> (which is also polymorphic)
// It wouldn't be polymorphic if Other would also be concretely bound to a specific data type.

let it1 = Paper "Magasine"
let it2 = Glass "Bottle"
let it3 = Glass "Jam"
let it4 = Other("Beer can", "Alumenium")
let it5 = Other("Bag", "Plastic")

let items = it1 :: it2 :: it3 :: it4 :: it5 :: []
let itemsList = [ it1; it2; it3; it4; it5 ]
// you have to add items to an empty list (otherwise they are just loose itmes and string concat doesn't work)
// two ways to construct a list

let fnPP (n, t) = $"{t} ({n})"

let res = fnPP ("Bag", "Plastic")



let ppTrashItem fnPP =
    function
    | Paper s -> $"Paper ({s})"
    | Glass g -> $"Glass ({g})"
    | Other o -> fnPP o

ppTrashItem fnPP item1
ppTrashItem fnPP item2
ppTrashItem fnPP item3
ppTrashItem fnPP item4

// > # 47 @"/Users/max/Library/Mobile Documents/com~apple~CloudDocs/Documen
// ts/UNI/MSc/SD/Spring 2025/FP/Exam/code/exam2022.fsx"
// - ppTrashItem fnPP item4;;
// val it: string = "Glass (Wine glass)"

// > # 46 @"/Users/max/Library/Mobile Documents/com~apple~CloudDocs/Documen
// ts/UNI/MSc/SD/Spring 2025/FP/Exam/code/exam2022.fsx"
// - ppTrashItem fnPP item3;;
// val it: string = "Shoe (Sneakers)"

// > # 45 @"/Users/max/Library/Mobile Documents/com~apple~CloudDocs/Documen
// ts/UNI/MSc/SD/Spring 2025/FP/Exam/code/exam2022.fsx"
// - ppTrashItem fnPP item2;;
// val it: string = "Paper (Newspaper)"

// > # 44 @"/Users/max/Library/Mobile Documents/com~apple~CloudDocs/Documen
// ts/UNI/MSc/SD/Spring 2025/FP/Exam/code/exam2022.fsx"
// - ppTrashItem fnPP item1;;
// val it: string = "Clothes (Shirt)"

let isPaper item =
    match item with
    | Paper _ -> true
    | _ -> false

//1.2
type TrashCan<'a> =
    | Empty
    | TrashItems of TrashItem<'a> * TrashCan<'a>
// a trashcan can be empty or filled with a item that is linked to a trashcan.
// a non-empty finite trashcan always end with an TrashItem

let rec addItem item tc =
    match tc with
    | Empty -> TrashItems(item, Empty)
    | TrashItems(i, tc') -> TrashItems(i, addItem item tc')

let addItem' item tc = TrashItems(item, tc)

addItem' item1 Empty

let ofList ts =
    List.foldBack addItem (List.rev ts) Empty
// foldBack takes a function which takes a type ('T) and a state (State) and returns the new state of the initial state

let tcEx = ofList items

let rec forAll fnP tc =
    match tc with
    | Empty -> true
    | TrashItems(i, tc) -> fnP i && forAll fnP tc

let rec forAll' fnP tc =
    match tc with
    | Empty -> true
    | TrashItems(i, tc') ->
        match fnP i with
        | true -> forAll' fnP tc'
        | false -> false
// immediatly returns when one of the items evaluate to false


forAll isPaper Empty
forAll isPaper tcEx


let getType item =
    match item with
    | Glass _ -> 1
    | Paper _ -> 0
    | Other _ -> -1

let isSameCategory item1 item2 = getType item1 = getType item2

let isSameCategory' i1 i2 =
    match i1, i2 with
    | Glass _, Glass _
    | Paper _, Paper _
    | Other _, Other _ -> true
    | _, _ -> false

forAll isPaper Empty
forAll isPaper tcEx

let rec isSorted tc =
    match tc with
    | Empty -> true
    | TrashItems(i, c) -> forAll' (isSameCategory' i) tc

isSorted tcEx

// 1.3
let rec fold f e tc =
    match tc with
    | Empty -> e
    | TrashItems(i, c) ->
        let res = f e i // the idea was good. Just had to define a intermediate result to feed to the rec call
        fold f res c


fold (fun n _ -> n + 1) 0 tcEx

let rec sort (tc: TrashCan<'a>) : TrashCan<'a> * TrashCan<'a> * TrashCan<'a> =
    match tc with
    | Empty -> Empty, Empty, Empty
    | TrashItems(i, rest) ->
        let paperTc, glassTc, otherTc = sort rest

        match i with
        | Paper s -> (TrashItems(Paper s, paperTc), glassTc, otherTc)
        | Glass s -> (paperTc, TrashItems(Glass s, glassTc), otherTc)
        | Other x -> (paperTc, glassTc, TrashItems(Other x, otherTc))

let (paperTc, glassTc, otherTc) = sort tcEx

isSorted paperTc

let rec filter fnP tc =
    match tc with
    | Empty -> Empty
    | TrashItems(i, rest) ->
        let filteredRest = filter fnP rest
        if fnP i then TrashItems(i, filteredRest) else filteredRest


filter isPaper tcEx

// 2.1

type Node<'a> =
    | Root of 'a
    | Link of 'a * Node<'a> ref

let mkRootElem a = ref (Root a)
let mkLinkElem a e = ref (Link(a, e))

let elemA = mkRootElem 'A'
let elemB = mkLinkElem 'B' elemA
let elemC = mkLinkElem 'C' elemB
let elemM = mkRootElem 'M'
let elemN = mkLinkElem 'N' elemM

// dereference the reference with !e
let getVal (e: Node<'a> ref) =
    match !e with
    | Root a -> a
    | Link(a, _) -> a

getVal elemA

let rec pathLength e =
    match !e with
    | Root _ -> 0
    | Link(_, e') -> 1 + pathLength e'

List.map pathLength [ elemA; elemB; elemC ]

// 2.2
let find e =
    match !e with
    | Root _ -> e
    | Link(_, e) -> e

find elemA = find elemB

find elemA = find elemB // true
find elemA = elemA // true
find elemB = elemA // true
find elemB = elemB // false
find elemC = find elemN // false

// the comparison is done on the reference of e. A and B both reference the same element, namely elemA
// A references itself since it is the root and B is the link pointing to elemA
// elemB = find elemB is false since the comparison is comparing different elements.
// the right side of the comparison is the element B in its entirety
// while find elemB returns the reference of elemB, int this case elemA
// elemC and elemN also do not reference the same element since they are in different linked lists

let union e1 e2 =
    let e1' = find e1
    let e2' = find e2

    match !e2' with
    | Root v -> e2' := Link(v, e1') // mutate the root to now be a link
    | Link _ -> failwith "union: got Link after find."

let rest = union elemA elemN


// 3.1
let rec f x =
    function
    | [] -> []
    | y :: ys when x = y -> f x ys
    | y :: ys when x <> y ->
        match f x ys with
        | ys' -> y :: ys'


// f is of type 'a -> 'a list -> 'a list when 'a : comparison
// there is a pattern matching on a list, so that has to be the second parameter.
// however there is no specific expectation from the items in the list other than they can be comparible to x (also of type 'a), this is called a type constraint
// making both input parameters polymorphic.

// function f removes all occurences of x in the list and returns the list where no elements e are e = x

f 1 [ 1; 2; 3 ] // [2; 3]
f 4 [ 1; 2; 3; 4 ] // [1; 2; 3]
f 4 [ 1; 2; 3; 4 ] // [1; 2; 3]

// f could better be called filter or remove since it returns a list void of occurences of x (x is removed / filtered out)

// 3.2
// the compiler complains because it doesn't recognize the when guard so it only sees [] and y :: ys ( twice )
// that is why it does not recognize when x = y & x <> y as full coverige and generates a warning

let rec f2 x =
    function
    | [] -> []
    | y :: ys ->
        if x = y then
            f x ys
        else
            match f x ys with
            | ys' -> y :: ys'

f2 1 [ 1; 2; 3 ] // [2; 3]
f2 4 [ 1; 2; 3; 4 ] // [1; 2; 3]
f2 4 [ 1; 2; 3; 4 ] // [1; 2; 3]

// 3.3
// it does not make use of an accumulator, but rather it keeps passing a list
// either it removes an elem (where elem = x) from the list
// or it recursively calls f on the tail of list
// the match f x ys clause waits until the whole recusion is completed when the list is empty
// then, it adds back all the y's that were removed in the first pattern matching

// A tail-recursive function is one where the last thing the function does is call itself.
// That way, the compiler (or runtime) can optimize the call by reusing the current stack frame,
// rather than pushing a new one.

let fA x list = // pass the list parameter in the main function to keep the same type
    let rec loop acc =
        function
        | [] -> List.rev acc
        | y :: ys -> if x = y then loop acc ys else loop (y :: acc) ys

    loop [] list

fA 1 [ 1; 2; 3 ] // [2; 3]
fA 4 [ 1; 2; 3; 4 ] // [1; 2; 3]
fA 4 [ 1; 2; 3; 4 ] // [1; 2; 3]

// 3.4
let fSeq x ys =
    seq {
        for i in ys do
            if x <> i then
                yield i
    }

fSeq 1 [ 1; 2; 3 ] // [2; 3]
fSeq 4 [ 1; 2; 3; 4 ] // [1; 2; 3]
fSeq 4 [ 1; 2; 3; 4 ] // [1; 2; 3]

// 4.1
type Field = ValField of int
type FieldMap = Map<string, Field>

exception NotFoundInMap of string

let fieldMap =
    Map.ofList [ ("A", ValField 42); ("B", ValField 43); ("C", ValField 0) ]

let fieldNames = List.map fst (Map.toList fieldMap)

// find element in map
let getField fieldMap fieldName =
    match Map.tryFind fieldName fieldMap with
    | Some v -> v
    | None -> raise (NotFoundInMap "element not found in map")

List.map (getField fieldMap) fieldNames

let getFieldValue (ValField x) = x

getFieldValue (ValField 43)

let lookupFieldValue fieldMap fieldName = getField fieldMap >> getFieldValue
// the function composition leaves out the final argument (it is now a curried function)
let lookupFieldValue' fieldMap fieldName =
    getFieldValue (getField fieldMap fieldName)

List.map (lookupFieldValue fieldMap) fieldNames

// method to update map / update value / update field
let updateField fieldMap fieldName newValue =
    match Map.tryFind fieldName fieldMap with
    | None -> raise (NotFoundInMap "this doesn't work")
    | Some n1 -> Map.add fieldName (ValField newValue) fieldMap

updateField fieldMap "A" 32


// 4.2
type Function = FnOneToOne of string * string * (int -> int)
type FuncMap = Map<string, Function>
let fnAddTwoAC = ("fnAddTwoAC", FnOneToOne("A", "C", fun a -> a + 2))
let fnNegateAB = ("fnNegateAB", FnOneToOne("A", "B", fun a -> -a))
let funcMap = Map.ofList [ fnAddTwoAC; fnNegateAB ]

exception NoValidFunction of string

let evalFn fieldMap (fn: Function) =
    match fn with
    | FnOneToOne(fromField, toField, fx) -> // make sure to match on reconrd type to 'unlock' the values inside
        let inputVal = lookupFieldValue' fieldMap fromField
        let newValue = fx inputVal
        updateField fieldMap toField newValue
    | _ -> raise (NoValidFunction "this function is not valid")

evalFn fieldMap (snd fnAddTwoAC)

// 4.3
let findFunctionsBySource sourceField (funcMap: Map<'a, Function>) =
    let lst = List.map (fun t -> snd t) (Map.toList funcMap)

    let rec loop acc xs =
        match xs with
        | [] -> List.rev acc
        | FnOneToOne(str1, str2, fn) :: xs' ->
            if sourceField = str1 then
                loop (FnOneToOne(str1, str2, fn) :: acc) xs'
            else
                loop acc xs'

    loop [] lst

findFunctionsBySource "A" funcMap
// my solution first selects the second element from each tuple that a map transformed to a list creates
// then it uses tail-recursion to accumulate all the elements that match the sourceField in a list

let rec evalField (fieldName: string) funcMap fieldMap =
    let lst = findFunctionsBySource fieldName funcMap

    let rec loop acc xs =
        match xs with
        | [] -> acc
        | FnOneToOne(str1, str2, fn) :: xs ->
            let inputVal = lookupFieldValue' fieldMap str1
            let acc' = Map.add str2 (ValField(fn inputVal)) acc
            loop acc' xs

    loop fieldMap lst

evalField "A" funcMap fieldMap
