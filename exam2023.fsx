// 1.1
type TwoLists = { listX: int list; listY: int list }

let rec f { listX = lX; listY = lY } =
    match (lX, lY) with
    | ([], []) -> f { listX = []; listY = [] }
    | ([], y :: ys) -> f { listX = [ y ]; listY = ys }
    | (x :: xs, []) -> { listX = x :: xs; listY = [] }
    | (xs, y :: ys) -> f { listX = y :: xs; listY = ys }

// TwoLists -> TwoList

// two empty list will continuously call f with the same input. The output value is the same as the input
// value so it will never get out of the loop.

// since f is tail-recursive with the two pararmeters as accumulator (Y is a decumulator)
// The stack will not grow like with normal recursion and therefore will only terminate until manually interupted

// it takes two lists (X and Y) and recursively moves the elements from Y list (from left to right) to the head of the X list
// until Y is empty and all elements are in X
// This leaves us with a list of all elements of Y reversed whereafter the original elements of x follow

// 1.2
let rec f2 { listX = lX; listY = lY } =
    match (lX, lY) with
    | ([], []) -> { listX = []; listY = [] }
    | ([], y :: ys) -> f2 { listX = [ y ]; listY = ys }
    | (x :: xs, []) -> { listX = x :: xs; listY = [] }
    | (xs, y :: ys) -> f2 { listX = y :: xs; listY = ys }

f2 { listX = []; listY = [] }

let rec f3 { listX = lX; listY = lY } =
    match (lX, lY) with
    | ([], y :: ys) -> f3 { listX = [ y ]; listY = ys }
    | (xs, []) ->
        if xs = [] then
            { listX = []; listY = [] }
        else
            { listX = xs; listY = [] }
    | (xs, y :: ys) -> f3 { listX = y :: xs; listY = ys }

let rec f3' { listX = lX; listY = lY } =
    match (lX, lY) with
    | ([], []) -> { listX = []; listY = [] }
    | (xs, y :: ys) -> f3' { listX = y :: xs; listY = ys }
    | (xs, []) -> { listX = xs; listY = [] }


f3' { listX = []; listY = [ 2; 3; 4 ] }

// 1.3
let infSeq = Seq.initInfinite id //(fun i -> i, -i)

infSeq |> Seq.take 10 |> Seq.toList

let mySeq =
    seq {
        for i in infSeq do
            yield i, -i
    }

Seq.take 1000 mySeq |> Seq.toList

let sumToZero = Seq.forall (fun (x, y) -> x + y = 0) (Seq.take 1000 mySeq)

//2.1
type Album = Elem list

and Elem =
    | PicElem of string
    | AlbumElem of string * Album

let pic1 = PicElem "Peter.jpg"
let pic2 = PicElem "Gitte.jpg"
let pic3 = PicElem "Mother.jpg"
let pic4 = PicElem "Farther.jpg"
let pic5 = PicElem "Hans.jpg"
let albumElem1 = AlbumElem("Familiy", [ pic3; pic4 ])
let albumElem2 = AlbumElem("School", [ pic5 ])
let albumElem3 = AlbumElem("All", [ pic1; albumElem1; pic2; albumElem2 ])
let album1 = [ albumElem3 ]

// 1. Does the value pic1 has type Elem?
// Yes, it is a Elem. PicElem is not a type - it is a constructor for the discriminated union of type Elem

// 2. Does the value albumElem1 has type Elem list?
// No. It is also of type Elem. The constructor here is AlbumElem that hold the name (string) and an Elem list

// 3. Does the value album1 has type Elem list?
// Yes. It is a elem wrapped in a list, which makes it an Elem list

// 4. Does the value album1 has type Album?
// Yes. Since Album is an alias for Elem list, they two types are fundamentally the same and album1 is of both types


let album2 =
    [ AlbumElem(
          "BasketTeams",
          [ AlbumElem("TeamA", [ PicElem "Kim.jpg"; PicElem "Per.jpg" ])
            PicElem "CoachAnna.jpg"
            AlbumElem("TeamB", [ PicElem "Ib.jpg"; PicElem "Ane.jpg" ])
            PicElem "CoachEmil.jpg" ]
      ) ]

// no they are monomorphic since eventually all elements are bound to a PicElem of type string
// when considering a finite Album.
// You can't initialize an Album with any other types and can't have empty Elem lists

let isAlbumElem elem =
    match elem with
    | AlbumElem(s, a) -> true
    | _ -> false

isAlbumElem albumElem3
isAlbumElem pic3

let newL i =
    System.Environment.NewLine + "".PadLeft(i, ' ')
// 2.2
let ppAlbum album =
    let rec loop alb acc n =
        match alb with
        | [] -> acc
        | PicElem s :: xs -> loop xs (acc + $"Pic: {s}" + newL (n + 1)) n
        | AlbumElem(s, alb) :: xs -> loop alb (acc + $"Album: {s}" + newL n) (n + 1) + loop xs (acc + newL n) (n + 1)

    loop album "" 0

let res = ppAlbum album1

printf "%A" res

// 2.3

let rec countAlbum album =
    album
    |> List.fold
        (fun (a, p) i ->
            match i with
            | PicElem _ -> (a, p + 1)
            | AlbumElem(_, subAlbum) ->
                let subPics, subAlbums = countAlbum subAlbum
                a + subAlbums + 1, p + subPics // +1 for this album
        )
        (0, 0)


countAlbum album1

// let rec fold f album acc =
//     match album with
//     | [] -> acc
//     | AlbumElem(_, alb) -> fold alb
//     | PicElem _ -> f

// 2.4
let rec flattenAlbum album =
    match album with
    | [] -> []
    | PicElem s :: xs -> s :: flattenAlbum xs
    | AlbumElem(s, alb) :: xs -> s :: flattenAlbum alb @ flattenAlbum xs

flattenAlbum album1

// this function prints the elements of the album in order of apperence
// In this case it first prints the album, then goes into the album and prints the
//PicElements in the album until it hits another album, then it goes inside that album.
// it only continues with the first album once all elements (albums and pictures) have been printed
// this is called pre-order traversal

//3.0
type MultiStack<'a, 'b> =
    { listA: ('a * int) list
      listB: ('b * int) list }

let exMS01 =
    { listA = [ ("Copenhagen", 2); ("Aarhus", 0) ]
      listB = [ (4000, 1) ] }

let exMS02 =
    { listA = [ (1000, 2); (42, 1) ]
      listB = [ ('Q', 3); ('C', 0) ] }

let mkMultiStack () = { listA = []; listB = [] }

let exMS03: MultiStack<int, int> = mkMultiStack ()

let size (ms: MultiStack<'a, 'b>) =
    match ms with
    | _ when ms = mkMultiStack () -> 0
    | { listA = (_, pA) :: xs
        listB = (_, pB) :: ys } -> if pA > pB then pA + 1 else pB + 1

size exMS01
size exMS02
size exMS03

let isEmpty ms = if size ms = 0 then true else false

isEmpty exMS01
isEmpty exMS03

// 3.2
let pushA e ms =
    match ms with
    | { listA = xs; listB = ys } ->
        { listA = (e, size ms) :: xs
          listB = ys }

pushA "Faxe" exMS01

let pushB e ms =
    match ms with
    | { listA = xs; listB = ys } ->
        { listA = xs
          listB = (e, size ms) :: ys }

pushB 4640 exMS01

let pop ms =
    match ms with
    | { listA = []; listB = [] } -> (None, None), ms
    | { listA = (vA, _) :: xs; listB = [] } -> (Some vA, None), { listA = xs; listB = [] }
    | { listA = []; listB = (vB, _) :: ys } -> (Some vB, None), { listA = []; listB = ys }
    | { listA = (vA, pA) :: xs
        listB = (vB, pB) :: ys } ->
        if pA > pB then
            (Some vA, None), { listA = xs; listB = (vB, pB) :: ys }
        else
            (Some vB, None), { listA = (vA, pA) :: xs; listB = ys }

// pop (pushB 4640 exMS01)


// 3.3
let rec chkList v lst =
    match lst with
    | [] -> true
    | (_, pA) :: xs -> if pA < v then chkList pA xs else false

let chkDecreasing ms =
    match ms with
    | { listA = []; listB = [] } -> true
    | { listA = xs; listB = [] } -> chkList 100000 xs && true
    | { listA = []; listB = ys } -> true && chkList 100000 ys
    | { listA = xs; listB = ys } -> chkList 100000 xs && chkList 100000 ys

let isStrictlyDecreasing lst =
    lst |> List.map snd |> List.pairwise |> List.forall (fun (a, b) -> b < a)

let chkDecreasing' ms =
    isStrictlyDecreasing ms.listA && isStrictlyDecreasing ms.listB


chkDecreasing' exMS01
chkDecreasing exMS01

let forAll fnA fnB ms =
    let rec loop lst fn =
        match lst with
        | [] -> true
        | (vA, _) :: xs -> fn vA && loop xs fn

    loop ms.listA fnA && loop ms.listB fnB

forAll (fun x -> String.length x > 2) ((<) 1000) exMS01

let map fnA fnB ms =
    let apply fn = List.map (fun (v, i) -> fn v, i)

    { listA = apply fnA ms.listA
      listB = apply fnB ms.listB }


// 4.1
let fSharp1 () =
    let x = 42.0
    let y = 45.0
    let z = x + y
    z

let fSharp2 () =
    let x = 42.0
    let c = 2.0
    let y = x * c
    let z = x * y
    z

// let fSharp3 () =
//     let x = 42.0
//     let c = 2.0
//     let y = x * c
//     let z = x * z
//     z

type Opr =
    | ADD
    | SUB
    | MUL

type target = string
type source = string

type FuncDef =
    | BIN_OP of Opr * target * source * source
    | CONST of target * float
    | SEQ of FuncDef list

type env = Map<string, float>

let fDef1 = SEQ [ CONST("x", 42.0); CONST("y", 45.0); BIN_OP(ADD, "z", "x", "y") ]

let fDef2 =
    SEQ
        [ CONST("x", 42.0)
          CONST("c", 2.0)
          BIN_OP(MUL, "y", "x", "c")
          BIN_OP(MUL, "z", "x", "y") ]

let fDef3 =
    SEQ
        [ CONST("x", 42.0)
          CONST("c", 2.0)
          BIN_OP(MUL, "y", "x", "c")
          BIN_OP(MUL, "z", "x", "z") ]

// here the compliner does not know what the evaluation of fDef3 will return
// because the types of operations have not yet been defined.

// Unlike actual F# code, the expression tree (FuncDef) allows representing invalid or
// ill-formed computations (like using a variable before it’s defined) without executing them.
// The error would only surface during interpretation or evaluation, not during construction.

let lookupEnv x env =
    match Map.tryFind x env with
    | None -> failwith "key not found"
    | Some s -> s

lookupEnv "x" (Map.ofList [ ("x", 3.0) ])

let addEnv x v env = Map.add x v env
addEnv "x" 3.0 Map.empty

// 4.2
let evalOpr (x: float) y opr =
    match opr with
    | ADD -> x + y
    | SUB -> x - y
    | MUL -> x * y


let rec evalFuncDef (env: Map<source, float>) fDef =
    match fDef with
    | BIN_OP(opr, tar, sor1, sor2) ->
        let sor1' = Map.find sor1 env
        let sor2' = Map.find sor2 env
        let intermediateResult = evalOpr sor1' sor2' opr
        addEnv tar intermediateResult env
    | CONST(t, f) -> addEnv t f env
    | SEQ xs -> List.fold evalFuncDef env xs // practical example of how to use fold instead of tail recurion
// let rec loop lst =
//     match lst with
//     | x :: xs' ->
//         evalFuncDef env x
//         loop xs'

// loop xs
evalFuncDef Map.empty fDef1
evalFuncDef Map.empty fDef2
evalFuncDef (Map.ofList [ ("z", 1.0) ]) fDef3

let f1 = ("z", fDef1)
let f2 = ("z", fDef2)
let f3 = ("z", fDef3)

let evalFunc (t, fDef) =
    fDef
    |> evalFuncDef Map.empty
    |> Map.toList
    |> List.find (fun (t', _) -> t = t')
    |> snd

evalFunc f1
evalFunc f2
evalFunc f3
