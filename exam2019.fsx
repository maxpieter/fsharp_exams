// 1.1

let infSeq3 =
    seq {
        for i in 0 .. (int) infinity do
            i * 3
    }

infSeq3 |> Seq.take 10 |> Seq.toList

let finSeq3 n = infSeq3 |> Seq.take n

let sumSeq3 n = Seq.sum (finSeq3 n)

//1.2

let seqMap2 f s1 s2 =
    seq {
        for x, y in Seq.zip s1 s2 do
            yield f x y
    }

// seqMap2 takes two sequences and returns the result of a function (f) applied to
// the i'th element in both sequences (s1 & s2).
// If one list is bigger than the other. It disregards the

let rest = seqMap2 (+) (finSeq3 9) (finSeq3 5) |> Seq.toList

Seq.zip (finSeq3 9) (finSeq3 5) |> Seq.toList

let swap (x, y) = (y, x)

// seqMap2 swap [ 1; 3; 3 ] [ 4; 5; 2 ]

// swap takes a tuple of two variables and swaps them around within the tuple
// the fuction provided does not work because it gives two argumetns, not a single tuple.

let fix f = fun x y -> f (x, y) // make a one argument function into two

seqMap2 (fix swap) [ 1; 3; 3 ] [ 4; 5; 2 ] |> Seq.toList


// 2.1
type TrieNode<'a when 'a: equality> = TN of 'a * bool * TrieNode<'a> list

let trie01: TrieNode<char> =
    TN('a', false, [ TN('n', false, [ TN('d', true, []) ]) ])

let trie03 =
    TN(
        'a',
        false,
        [ TN('n', true, [ TN('d', true, []) ])
          TN('d', false, [ TN('d', true, []) ])
          TN('t', true, []) ]
    )

let trie04 =
    TN(
        'a',
        false,
        [ TN('n', true, [ TN('d', true, []) ])
          TN('d', false, [ TN('d', true, []) ])
          TN('t', true, [ TN('x', false, []) ]) ]
    )

// trie04 is of type TN<char> because all 'a in trie04 are concretly bound to char

exception TrieError of string

// 2.2
let rec numLetters (TN(_, _, children)) = 1 + (children |> List.sumBy numLetters)

let rec numLetters1 (tn: TrieNode<'a>) =
    match tn with
    | TN(_, _, []) -> 0
    | TN(_, _, [ tn2 ]) -> 1 + numLetters tn2

numLetters trie04

let rec numWords (TN(_, isWord, children)) =
    let current = if isWord then 1 else 0
    current + (children |> List.sumBy numWords)

numWords trie04

let rec exists ls t =
    match ls, t with
    | [ x ], TN(c, isWord, _) -> c = x && isWord
    | x :: xs, TN(c, _, children) when c = x ->
        match xs with
        | [] -> false // this case is actually already handled above
        | y :: ys ->
            match List.tryFind (fun (TN(c', _, _)) -> c' = y) children with
            | Some child -> exists (y :: ys) child
            | None -> false
    | _ -> false

exists [ 'a'; 't'; 'x' ] trie04
// stupid hard...

let rec chkTrie (TN(_, isWord, children)) =
    match children with
    | [] -> isWord // A leaf must mark the end of a word
    | _ -> List.forall chkTrie children // All subtrees must also be valid

chkTrie trie01

let rec map f (TN(v, isWord, children)) =
    TN(f v, isWord, List.map (map f) children)

map (string) trie03
// TN
// ("a", false,
//  [TN ("n", true, [TN ("d", true, [])]);
//   TN ("d", false, [TN ("d", true, [])]); TN ("t", true, [])])

// it turns all chars into a string
// 3.1
let rec F m i n k =
    match k with
    | 0 -> m
    | _ -> F m i n (k - 1) * (1.0 + i / n)

#time
F 100000.0 0.1 1.0 100000
#time
// my implementation is not tail recursive as it first has to reach the base case (k =< 0) in order to start adding all other calls to the first call
// below a tail-recursive version
let FA (m: float) i n k =
    let rec loop s acc =
        match s with
        | s when s = k -> acc
        | _ -> loop (s + 1) (acc * (1.0 + i / n))

    loop 0 m // no loop variables in the actual funtion

#time
FA 100000.0 0.1 1.0 100000
#time

// as you can see the tail recursive function is slightly faster (0.001 sec faster :) )

let tabulate f start step stop =
    seq {
        for i in start..step..stop do
            yield i, f i // yield! on a value of type float, but yield! expects something that implements IEnumerable, i.e., a sequence.
    }
    |> Seq.toList

tabulate (F 100.0 0.1 1.0) 0 2 4

let prettyPrint xs =
    printfn "  x   | f(x) "
    printfn "------+------"

    for (x, y) in xs do
        printfn $"    {x} | {y} "

prettyPrint [ (0, 100.0); (2, 121.0); (4, 146.41) ]


// 4.1
let dt ((d: int), m, y) = System.DateTime(y, m, d)
exception Error of string

type Position =
    | Stock of string
    | Cash of float

type Action =
    | Aquire of System.DateTime * Position
    | Give of System.DateTime * Position
    | Scale of int * Action
    | All of Action list

let ex1 =
    Scale(
        100,
        All[Aquire(dt (1, 2, 2018), Stock "APPLE")
            Give(dt (1, 2, 2018), Cash 300.3)]
    )

let sellApple =
    Scale(
        100,
        All[Aquire(dt (1, 3, 2018), Cash 400.4)
            Give(dt (1, 3, 2018), Stock "APPLE")]
    )

let price (s, d) =
    match s, d with
    | "APPLE", (1, 2, 2018) -> 300.3
    | "APPLE", (1, 3, 2018) -> 400.4
    | "ISS", (1, 2, 2018) -> 150.0
    | "ISS", (1, 3, 2018) -> 200.2
    | "TIVOLI", (1, 2, 2018) -> 212.0
    | "TIVOLI", (1, 3, 2018) -> 215.2
    | _, _ -> raise (Error "not in lookup table") //ensure consistent output (safe exception)

printfn "%A" (price ("APPLE", (1, 2, 2018)))

// 4.2
let buyStock n s d =
    Scale(
        n,
        All[Aquire(d, Stock s)
            Give(d, Cash(price (s, (d.Day, d.Month, d.Year))))]
    )

buyStock 100 "APPLE" (dt (1, 2, 2018))

let receiveCash (c: float) (d: System.DateTime) =
    Aquire(dt (d.Day, d.Month, d.Year), Cash c)

receiveCash 100000.0 (dt (1, 2, 2018))

// 4.3
let actions =
    let d1 = dt (1, 2, 2018)
    let d2 = dt (1, 3, 2018)

    All
        [ receiveCash 100000.0 d1
          buyStock 100 "APPLE" d1
          buyStock 200 "ISS" d1
          buyStock 50 "TIVOLI" d2 ]


type stockEnv = Map<string, int>

let updStock s n m =
    match Map.tryFind s m with
    | None -> Map.add s n m
    | Some n1 -> Map.add s (n + n1) m

type env = float * stockEnv
let emptyEnv = (0.0, Map.empty)

let updEnv scaling (cash, stockEnv) pos : env =
    match pos with
    | Stock str ->
        let updatedStock = updStock str scaling stockEnv
        cash, updatedStock
    | Cash flt ->
        let updatedCash = cash + float scaling * flt
        updatedCash, stockEnv

updEnv 100 emptyEnv (Cash 100.0)
updEnv 100 emptyEnv (Stock "APPLE")

let execA action env =
    let rec exec scaling env =
        function
        | All xs -> List.fold (fun acc act -> exec scaling acc act) env xs // List.fold uses accumulator
        | Scale(i, act) -> exec (i * scaling) env act
        | Aquire(_, p) -> updEnv scaling env p // env is a tuple
        | Give(_, p) -> updEnv -scaling env p

    exec 1 env action

execA actions emptyEnv
