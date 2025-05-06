type item = { id: int; name: string; price: float }
type register = item list

// 1.1
let register =
    [ { id = 1; name = "Milk"; price = 8.75 }
      { id = 2
        name = "Juice"
        price = 16.25 }
      { id = 3
        name = "Rye Bread"
        price = 25.00 }
      { id = 4
        name = "White Bread"
        price = 18.50 } ]

exception Register of string

// 1.2
let getItemById01 id r =
  match List.filter (fun {id=id';name=_;price=_} -> id=id') r with
    [] ->  raise (Register ("Item with id " + id.ToString() + " does not exists."))
  | x::xs -> x
let rec getItemById02 i = function
    | [] -> raise (Register ("Item with id " + id.ToString() + " does not exists."))
    | ({id=id';name=_;price=_} as item) :: rs -> if i = id' then item else getItemById02 i rs

// 1.3
let nextId r =
    match r with
    | [] -> 1
    | _ -> 1 + List.length r

let nextId01 r = List.fold (fun maxId {id=id;name=_;price=_} -> if maxId < id then id else maxId) 1 r + 1
let nextId01' r = List.fold (fun maxId {id=id;name=_;price=_} -> if maxId < id then id+1 else maxId) 1 r
let nextId01'' r = List.fold (fun maxId {id=id;name=_;price=_} -> max id maxId) 1 r + 1


let nextIDWithFold r =
    List.fold (fun acc item -> max acc item.id) 0 r + 1

nextId register
nextIDWithFold register

let rec nextID02 = function
    [] -> 1
    | {id=id; name=_; price=_} :: rs -> let maxId = nextID02 rs 
                                        if maxId < id then id + 1 else maxId

// 1.4
let addItem n p (r: register) : register =
    r @ [ { id = nextId r; name = n; price = p } ]
// or { id = nextId r; name = n; price = p } :: r       BETTER PERFORMANCE

addItem "Cookies" 10.99 register

// 1.5
// delete item from list by id
let deleteItem i r =
    List.filter (fun item -> item.id <> i) r

let rec deleteItem01 id = function
  [] -> []
| ({id=id';name=_;price=_} as x)::xs -> let rest = deleteItem01 id xs
                                        if id=id' then rest else x::rest

// 1.6
let uniqueRegister (r: register) =
    List.forall (fun item -> List.filter (fun x -> x.id = item.id) r |> List.length = 1) r


let rec uniqueRegisterJakob r =
    match r with
    | [] -> true
    | x :: xs ->
        if List.forall (fun y -> x.id <> y.id) xs then
            uniqueRegisterJakob xs
        else
            false

let uniqueRegister' r = let ids = List.map (fun {id=id; name=_;price=_} -> id) r 
                                    List.length ids = List.length (Set.toList (Set.ofList ids))

uniqueRegister register


// 1.7
let itemsInPriceRange p d r =
    List.filter (fun x -> x.price >= float p - d && x.price <= float p + d) r

let itemsInPriceRange01 p d r =
  let minP, maxP = p-d, p+d 
  List.fold (fun r' ({id=_;price=p';name=_} as x) -> if p' >= minP && p' <= maxP then x::r' else r') [] r


itemsInPriceRange 15 5 register

// 2.1
#time

let rec f n m =
    if m = 0 then n else n * f (n + 1) (m - 1)

// f computes (n + m - 1)! / (n - 1)!
// This is a recursive multiplication.
// It starts at n and performs m multiplications, incrementing n each time.

// 2.2

let f' n m =
    let rec helper acc n m =
        if m = 0 then acc else helper (acc * n) (n + 1) (m - 1)

    helper 1 n m

let res1 = f 100000000 10000000
let res2 = f' 10 10

// 2.3
let rec z xs ys =
    match (xs, ys) with
    | ([], []) -> []
    | (x :: xs, []) -> (x, x) :: (z xs ys)
    | ([], y :: ys) -> (y, y) :: (z xs ys)
    | (x :: xs, y :: ys) -> (x, y) :: (z xs ys)

// type: 'a list -> a' list -> ('a * 'a) list

let listA = [ 2; 3; 4; 5 ]
let listB = [ 10; 11; 12; 13 ]
let listC = []

z listA listB
z listA listC

// This function creates a new list of tuples from two input lists.
// if both lists are non-empty, the tuple consists of the first element from each list
// if one of the lists is empty, the function duplicates the remaining elements from the non-empty list

// 2.4

let rec s xs ys =
    match (xs, ys) with
    | ([], []) -> []
    | (xs, []) -> xs
    | ([], ys) -> ys
    | (x :: xs, y :: ys) -> x :: y :: s xs ys

s listA listB

// type: 'a list -> 'a list -> 'a list
// s takes two lists as input and creates a new list by alternating between the head of each list (x first, then y) and adding it to the new list
// if any of the two lists are empty, it returns the rest of the list that is non-empty
// zipping and interleaving the lists

// 2.5
let rec sC xs ys cont =
    match (xs, ys) with
    | [], [] -> cont []
    | xs, [] -> cont xs
    | [], ys -> cont ys
    | x :: xs', y :: ys' -> sC xs' ys' (fun result -> cont (x :: y :: result))

s listA listB

// 3.1
// type Latex<'a> =
//     | Section of string * 'a * Latex<'a>
//     | Subsection of string * 'a * Latex<'a>
//     | Text of string * Latex<'a>
//     | End

let text1 =
    Section(
        "Introduction",
        None,
        Text(
            "This is an introduction to ...",
            Subsection("A subsection", None, Text("As laid out in the introduction we ...", End))
        )
    )

let addSecNumbers (doc: Latex<'a>) : Latex<float option> =
    let rec add secNo subNo doc =
        match doc with
        | End -> End, secNo, subNo
        | Text(text, next) ->
            let next', secNo', subNo' = add secNo subNo next
            (Text(text, next'), secNo', subNo')
        | Section(title, _, next) ->
            let newSec = secNo + 1.0
            let next', secNo', _ = add newSec 0.0 next // reset subsection count
            (Section(title, Some newSec, next'), secNo', 0.0)
        | Subsection(title, _, next) ->
            let newSub = subNo + 1.0
            let next', secNo', subNo' = add secNo newSub next
            (Subsection(title, Some(secNo + newSub / 10.0), next'), secNo', newSub)

    let result, _, _ = add 0.0 0.0 doc
    result


let text2 =
    Section(
        "Introduction",
        None,
        Text(
            "This is an introduction to ...",
            Subsection(
                "A subsection",
                None,
                Text(
                    "As laid out in the introduction we ...",
                    Subsection(
                        "Yet a subsection",
                        None,
                        Section("And yet a section", None, Subsection("A subsection more...", None, End))
                    )
                )
            )
        )
    )

addSecNumbers text2

// 3.3
// type: Latex<'a> -> Latex<float option>

// 3.4
type Latex<'a> =
    | Section of string * 'a * Latex<'a>
    | Subsection of string * 'a * Latex<'a>
    | Label of string * Latex<'a>
    | Text of string * Latex<'a>
    | Ref of string * Latex<'a>
    | End

let text3 =
    Section(
        "Introduction",
        "1",
        Label(
            "intro.sec",
            Text(
                "In section",
                Ref(
                    "subsec.sec",
                    Text(
                        " we describe ...",
                        Subsection(
                            "A subsection",
                            "1.1",
                            Label(
                                "subsec.sec",
                                Text(
                                    "As laid out in the introduction, Section ",
                                    Ref("intro.sec", Text(" we ...", End))
                                )
                            )
                        )
                    )
                )
            )
        )
    )

let rec buildLabelEnv doc = 
    addSecNumbers text3
    let rec add doc =
        match doc with
        | End -> Map.empty
        | Text(text, next) ->
        | Section(title, _, next) ->
        | Subsection(title, _, next) ->

