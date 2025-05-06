// 1.1
type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

let ex3 =
    HP(1, HP(2, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))

// The type is Heap<int> it is monomorphic since the heap is constructed with only integers.
// 'a is here concretely set to int and type inference resolves the type of ex3 to Heap<int>
// The type definition of Heap<'a>is polymorphic. However the value is monomorphic because 'a is specifically set to int in this instance

let empty = EmptyHP

exception HeapError of string


// 1.2
let isEmpty h =
    match h with
    | EmptyHP -> true
    | _ -> false

isEmpty ex3

let rec size h =
    match h with
    | EmptyHP -> 0
    | HP(_, left, right) -> 1 + size left + size right

size ex3

let find h =
    match h with
    | EmptyHP -> raise (HeapError "Heap cannot be empty")
    | HP(value, _, _) -> value

find ex3

let rec chkHeapProperty h =
    match h with
    | EmptyHP -> true
    | HP(value, left, right) ->
        let leftOK =
            match left with
            | EmptyHP -> true
            | HP(lv, _, _) -> value <= lv && chkHeapProperty left

        let rightOK =
            match right with
            | EmptyHP -> true
            | HP(rv, _, _) -> value <= rv && chkHeapProperty right

        leftOK && rightOK

chkHeapProperty ex3

// 1.3
let rec map (f: 'a -> 'b) (h: Heap<'a>) =
    match h with
    | EmptyHP -> EmptyHP
    | HP(value, left, right) -> HP(f value, map f left, map f right)

// f is applied in a pre-order traversal. First the node, then recusively the left subtree whereafter the right subtree.

let f = fun x -> x % 2

let rest = map f ex3
chkHeapProperty (map f ex3)

printfn "%A" rest

// 2.1
let random =
    let rnd = System.Random()
    fun () -> rnd.Next(1, 10000)

let genRandoms n = Array.init n (fun _ -> random ()) // use () to call an function with input type unit


// let genRandomsP n =
//     let rnd = System.Random()
//     let lockObj = obj ()
//     Array.Parallel.init n (fun _ -> lock lockObj (fun () -> rnd.Next(1, 10000)))


#time
let res = genRandoms 100000000
// let resP = genRandomsP 100000000

// 2.2
let split xs =
    let mid = List.length xs / 2
    List.splitAt mid xs

split [ 11; 456; 23; 456; 234 ] // an uneven number of items splits the list in a way that the first list is n-1 items compared to the second
split [ 22 ] //
split [ 22; 746; 931; 975; 200 ]


let indivisible xs =
    if List.length xs < 2 then true else false

indivisible [ 23; 34; 45 ]

let rec merge xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xt, y :: yt -> if x <= y then x :: merge xt ys else y :: merge xs yt


// 2.3
let rec divideAndConquer split merge indivisible p =
    if indivisible p then
        p
    else
        let p1, p2 = split p // divides the problem into subproblems p1 and p2
        let r1 = divideAndConquer split merge indivisible p1 // do this until p is indivisible
        let r2 = divideAndConquer split merge indivisible p2
        merge r1 r2

let res3 = divideAndConquer split merge indivisible [ 22; 746; 931; 975; 200 ]

printf "%A" res3

// 3.1

let triNum = Seq.initInfinite (fun n -> n * (n + 1) / 2)

let triNumC = triNum |> Seq.cache

// 3.2
let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s)) (filterOddIndex (Seq.skip 2 s))

filterOddIndex triNum

let myFilterOddIndex s = Seq.delay (fun () -> filterOddIndex s)

myFilterOddIndex triNum

// 3.3
let rec zipSeq s1 s2 =
    seq {
        if not (Seq.isEmpty s1 || Seq.isEmpty s2) then
            yield Seq.head s1, Seq.head s2
            yield! zipSeq (Seq.skip 1 s1) (Seq.skip 1 s2)
    }



// 4.1
