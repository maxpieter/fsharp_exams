type mymap<'a, 'b> = MyMap of list<'a * 'b>

let ex1 = MyMap [ ('A', 65); ('B', 66); ('C', 67) ]
let ex1' = MyMap [ ('C', 67); ('A', 65); ('B', 66) ]

// 1.1
let dice1 = MyMap [ 1, 4; 2, 2; 3, 3; 4, 2; 5, 2; 6, 2 ]
let dice2 = MyMap [ 1, 4; 2, 2; 3, 3; 4, 3; 5, 5; 6, 3 ]

// ex1 is of type MyMap<char,int> since the compiler recognizes the key to be defined as a char, mapped to an int
// dice1 is of type MyMap<int, int> as eyes are mapped to frequency, both in integers

let emptyMap () = MyMap []

let size (MyMap m) =
    let rec helper m acc =
        match m with
        | [] -> acc
        | x :: xs -> helper xs (acc + 1)

    helper m 0

let size1 (MyMap m) = List.length m

size1 (emptyMap ())

// 1.2
let isEmpty (MyMap m) =
    match m with
    | [] -> true
    | _ -> false

let isEmpty1 (MyMap m) = List.isEmpty m

isEmpty1 (emptyMap ())

let tryFind k (MyMap m) =
    let rec helper k m =
        match m with
        | [] -> None
        | (a, b) :: xs -> if a = k then Some(a, b) else helper k xs

    helper k m

let tryFind1 k (MyMap m) =
    List.tryPick (fun (a, b) -> if a = k then Some(a, b) else None) m

tryFind1 1 dice1

let remove k (MyMap m) =
    MyMap(List.filter (fun (x, _) -> x <> k) m)

remove 'G' ex1

let add k v (MyMap m) =
    MyMap((k, v) :: List.filter (fun (a, _) -> a <> k) m)

add 'B' 5 ex1

// 1.3
let upd (f: 'f -> 'b -> 'c) k v (m: mymap<'a, 'b>) =
    match tryFind k m with
    | None -> add k v m
    | Some(_, v') -> add k (f v v') m

upd (+) 'A' 65 ex1


let map (f: 'k -> 'v -> 'v2) (MyMap m: mymap<'k, 'v>) : mymap<'k, 'v2> =
    MyMap(List.map (fun (k, v) -> (k, f k v)) m)

map (fun k v -> v + 2) ex1

let fold f s (MyMap(m: ('k * 'v) list): mymap<'k, 'v>) : mymap<'k, 'v> =
    MyMap(List.fold (fun acc (k, v) -> f acc k v) s m)


// 2.1
let even n =
    match n with
    | _ when n % 2 = 0 -> true
    | _ -> false

let collatz n = if even n then n / 2 else 3 * n + 1

collatz 45

let collatz' n =
    match n with
    | _ when n <= 0 -> raise (System.Exception "n is zero or less")
    | _ -> collatz n

collatz' 1

// 2.2

let rec applyN f n N =
    if N = 0 then
        []
    else
        let next = f n
        next :: applyN f next (N - 1)

applyN collatz 42 8

let applyUntilOne f n =
    let rec loop acc v =
        if v = 1 then acc else loop (acc + 1) (f v)

    loop 0 n

applyUntilOne collatz 48

// 2.3
let rec mySeq f x =
    seq {
        yield x
        yield! mySeq f (f x)
    }

mySeq collatz 42 |> Seq.take 9 |> Seq.toList

// returns an infinite sequence by recursively applying f to the last result
// in the current setup, it will quickly reach the 4 -> 2 -> 1 loop
// this will not terminate unless instructed like above with Seq.take


let g x = x * 2

mySeq g 1 |> Seq.take 10 |> Seq.toList


// 3.1

type name = string
type quantity = float
type date = int * int * int
type price = float

type transType =
    | Buy
    | Sell

type transData = date * quantity * price * transType
type trans = name * transData


let ts: trans list =
    [ "ISS", ((24, 02, 2014), 100.0, 218.99, Buy)
      "Lego", ((16, 03, 2015), 250.0, 206.72, Buy)
      "ISS", ((23, 02, 2016), 825.0, 280.23, Buy)
      "Lego", ((08, 03, 2016), 370.0, 280.23, Buy)
      "ISS", ((24, 02, 2017), 906.0, 379.46, Buy)
      "Lego", ((09, 11, 2017), 80.0, 360.81, Sell)
      "ISS", ((09, 11, 2017), 146.0, 360.81, Sell)
      "Lego", ((14, 11, 2017), 140.0, 376.55, Sell)
      "Lego", ((20, 02, 2018), 800.0, 402.99, Buy)
      "Lego", ((02, 05, 2018), 222.0, 451.80, Sell)
      "ISS", ((22, 05, 2018), 400.0, 493.60, Buy)
      "ISS", ((19, 09, 2018), 550.0, 564.00, Buy)
      "Lego", ((27, 03, 2019), 325.0, 625.00, Sell)
      "ISS", ((25, 11, 2019), 200.0, 680.50, Sell)
      "Lego", ((18, 02, 2020), 300.0, 720.00, Sell) ]

let addTransToMap (t: trans) (m: Map<name, transData list>) =
    let (n, d) = t

    match Map.tryFind n m with
    | Some lst -> Map.add n (List.rev (d :: lst)) m
    | None -> Map.add n [ d ] m

let m1 = addTransToMap ("ISS", ((24, 02, 2014), 100.0, 218.99, Buy)) Map.empty
let m2 = addTransToMap ("ISS", ((22, 05, 2018), 400.0, 493.60, Buy)) m1

let shares = List.foldBack addTransToMap ts Map.empty

// 3.2
let accTrans (tq: float, avg: float) ((d, q, p, tType): transData) =
    match tType with
    | Buy -> tq + q, (avg * tq + q * p) / (tq + q)
    | Sell -> tq - q, avg

let quantityAndAvgPrice ts = List.fold accTrans (0.0, 0.0) ts
quantityAndAvgPrice [ (24, 02, 2014), 100.0, 218.99, Buy; (23, 02, 2016), 825.0, 280.23, Buy ]

let res = Map.map (fun _ v -> quantityAndAvgPrice v) shares
// fun _ v is the same as fun (key, value) but we ignore the key because we only want to run quantityAndAvgPrice on v
// Map.map to transform each element in a map with a given function

// 4.1
let rec dup =
    function
    | [] -> []
    | x :: xs -> x :: x :: dup xs

dup [ 1; 2; 3; 4 ]
// it recursively takes the head of the list and adds it twice to a new list
// whereafter dup gets called on the rest of the list

let dupA lst =
    let rec loop acc =
        function
        | [] -> List.rev acc
        | x :: xs -> loop (x :: x :: acc) xs

    loop [] lst // make sure to call a param of the main function here if not present

dupA [ 1..1000 ]

// 4.2
let replicate2 id = seq { for i in 1..2 -> [ id ] }

Seq.toList (replicate2 4)

let dupSeq =
    seq {
        for i in 1 .. (int) infinity ->
            i
            i
    }

dupSeq |> Seq.take 10000 |> Seq.toList

// 4.3
let dupSeq2 (s: seq<int>) =
    seq {
        for i in s do
            yield i
            i
    }

dupSeq2 [ 1; 1; 2; 2 ] |> Seq.toList
