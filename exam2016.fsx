// 1.1
type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>

let ex = MSet(Map.ofList [ ("a", 1); ("b", 2); ("c", 1) ])

let wrong = MSet(Map.ofList [ ("a", 0); ("b", 2); ("c", 1) ])

let diceSet = MSet(Map.ofList [ (1, 2); (2, 1); (3, 5); (5, 2); (6, 2) ])

// the type is a Multiset<int>, the polymorphic type as now become monomorphic since 'a is concretly implemented with an int

// You can implement these functions with the MS as we have defined it here since it is polymorphic.
// As in, you can concretly implement 'a as a function float->float as long as you still include the integer since this part is not polymorphic
// Resulting in Map<(float->float) * int>

//1.2

let newMultiset () = MSet(Map.empty)

let isEmpty (MSet ms) =
    match Map.toList ms with
    | [] -> true
    | _ -> false

isEmpty (newMultiset ())

//1.3
let add k (MSet ms) = // add an element to a multiset
    let updated =
        match Map.tryFind k ms with
        | Some count -> Map.add k (count + 1) ms
        | None -> Map.add k 1 ms

    MSet updated

add "a" ex

let del e (MSet ms) : Multiset<'a> = // delete element from multiset
    let toBeDel =
        match Map.tryFind e ms with
        | Some c -> Map.remove e ms
        | None -> ms

    MSet toBeDel

del "c" ex

// 1.4
let toList (MSet ms) =
    ms |> Map.toList |> List.collect (fun (k, v) -> List.replicate v k)

toList ex // transforms Map to list with value amount of keys in the list



let fromList xs =
    let rec loop (acc: Multiset<'a>) remaining =
        match remaining with
        | [] -> acc
        | x :: xxs -> loop (add x acc) xxs

    loop (newMultiset ()) xs

fromList [ "a"; "a"; "b" ]

// 1.5
let map f (MSet ms) = // function applied to all items in map
    ms |> Map.toList |> List.map (fun (k, v) -> (f k, v)) |> Map.ofList |> MSet

map (fun (c: string) -> c.ToUpper()) ex

let fold f a (MSet ms) = // fold implemented for a map with accumulator
    ms |> Map.toList |> List.rev |> List.fold (fun acc (k, _) -> k + acc) ""

fold (fun acc e -> acc + e) "" ex

//1.6
let union (MSet ms1) (MSet ms2) = // the union of a multiset (adds two sets together)
    let combined =
        Map.fold
            (fun acc key value ->
                match Map.tryFind key acc with
                | Some existing -> Map.add key (existing + value) acc
                | None -> Map.add key value acc)
            ms1
            ms2

    MSet combined

union ex ex

// 1.7
let minus (MSet ms1) (MSet ms2) =
    let deleted =
        Map.fold
            (fun acc key count2 ->
                match Map.tryFind key acc with
                | Some count1 ->
                    let diff = count1 - count2

                    if diff > 0 then
                        Map.add key diff acc
                    else
                        Map.remove key acc
                | None -> acc)
            ms1
            ms2

    MSet deleted

minus ex ex

// 2.1

let rec f n = if n < 10 then "f" + g (n + 1) else "f"
and g n = if n < 10 then "g" + f (n + 1) else "g"

f 5

// all even numbers under 10 will result in a string ending in an "f"
// all numbers >= 10 will also evaluate to a string ending in "f"

g 4 // "gfgfgfg"

// No that is impossible. Since n is monomorphic (by int) and continuously adding 1 to any int
// (no matter how negative) will eventually result in n = 10, whereafter the function f terminates

// 2.2

let gA n =
    let rec loop acc newN =
        if n < 10 then loop (acc + "f") (newN + 1) else acc + "g"

    loop "" n

// 3.1
let myFinSeq (n, m) =
    seq {
        for i in [ 0..n ] do
            yield!
                seq {
                    for j in [ 0..m ] do
                        yield j
                }
    }

myFinSeq (1, 2) |> Seq.toList

// myFinSeq returns the sequence from 0 to any int m, n many times (thanks to yield!)

// no, you can only an n amount of sequences from 0 to m. In this case it does not work because the sequence 0; 1 is missing the 2

// 3.2
let myFinSeq2 (n, m) =
    seq {
        for i in [ 0..n ] do
            i,
            seq {
                for j in [ 0..m ] do
                    yield j
            }

    }


myFinSeq2 (1, 2) |> Seq.toList

// 4.1
type Row = int
type Col = char
type CellAddr = Row * Col

type ArithOp =
    | Add
    | Sub
    | Mul
    | Div

type RangeOp =
    | Sum
    | Count

type CellDef =
    | FCst of float
    | SCst of string
    | Ref of CellAddr
    | RangeOp of CellAddr * CellAddr * RangeOp
    | ArithOp of CellDef * ArithOp * CellDef

type CellValue =
    | S of string
    | F of float

type Sheet = Map<CellAddr, CellDef>

let header =
    [ ((1, 'A'), SCst "#EYES")
      ((1, 'B'), SCst "1")
      ((1, 'C'), SCst "2")
      ((1, 'D'), SCst "3")
      ((1, 'E'), SCst "4")
      ((1, 'F'), SCst "5")
      ((1, 'G'), SCst "6")
      ((1, 'H'), SCst "Total") ]

let result =
    [ ((2, 'A'), SCst "RESULT")
      ((2, 'B'), FCst 2.0)
      ((2, 'C'), FCst 1.0)
      ((2, 'D'), FCst 5.0)
      ((2, 'E'), FCst 0.0)
      ((2, 'F'), FCst 2.0)
      ((2, 'G'), FCst 2.0)
      ((2, 'H'), RangeOp((2, 'B'), (2, 'G'), Sum)) ]

let calcPct col =
    ArithOp(FCst 100.0, Mul, ArithOp(Ref(2, col), Div, Ref(2, 'H')))

let pct =
    [ ((3, 'A'), SCst "PCT")
      ((3, 'B'), calcPct 'B')
      ((3, 'C'), calcPct 'C')
      ((3, 'D'), calcPct 'D')
      ((3, 'E'), calcPct 'E')
      ((3, 'F'), calcPct 'F')
      ((3, 'G'), calcPct 'G')
      ((3, 'H'), calcPct 'H') ]

let dice = Map.ofList (header @ result @ pct)

let heightsB =
    [ ((4, 'B'), SCst "NAME")
      ((5, 'B'), SCst "Hans")
      ((6, 'B'), SCst "Trine")
      ((7, 'B'), SCst "Peter")
      ((8, 'B'), SCst "")
      ((9, 'B'), RangeOp((5, 'B'), (7, 'B'), Count)) ]

let heightsC =
    [ ((4, 'C'), SCst "NAME")
      ((5, 'C'), FCst 167.40)
      ((6, 'C'), FCst 162.30)
      ((7, 'C'), FCst 179.70)
      ((8, 'C'), SCst "")
      ((9, 'C'), ArithOp(RangeOp((5, 'C'), (7, 'C'), Sum), Div, Ref(9, 'B'))) ]

let heights = Map.ofList (heightsB @ heightsC)

// 4.2
// 1
let getF =
    function
    | F f -> f
    | S s -> failwith "getF: expecting a float but got a string"

let evalRangeOp xs op =
    match op with
    | Sum -> List.fold (fun acc x -> acc + getF x) 0.0 xs
    | Count -> float (List.length xs)

evalRangeOp [ F 33.0; F 32.0 ] Sum
evalRangeOp [] Sum
evalRangeOp [ F 23.0; S "Hans" ] Sum
evalRangeOp [ F 23.0; S "Hans" ] Count

// 2
let evalArithOp v1 v2 op =
    match op with
    | Add -> getF v1 + getF v2
    | Sub -> getF v1 - getF v2
    | Mul -> getF v1 * getF v2
    | Div -> getF v1 / getF v2

evalArithOp (F 33.0) (F 32.0) Sub
evalArithOp (S "Hans") (F 1.0) Add

// 4.3
let rangeToList (r1, c1) (r2, c2) : CellAddr list =
    [ for i in r1..r2 do
          for j in int c1 .. int c2 do
              yield (i, char j) ]

let rec evalValue (v: CellDef) (sheet: Sheet) =
    match v with
    | FCst f -> F f
    | SCst s -> S s
    | Ref ca -> evalCell ca sheet
    | RangeOp((r1, c1), (r2, c2), op) ->
        let cellAddrs = rangeToList (r1, c1) (r2, c2)
        let values = List.map (fun addr -> evalCell addr sheet) cellAddrs
        F(evalRangeOp values op)
    | ArithOp(v1, op, v2) ->
        let lhs = evalValue v1 sheet
        let rhs = evalValue v2 sheet
        F(evalArithOp lhs rhs op)

and evalCell (ca: CellAddr) (sheet: Sheet) =
    match Map.tryFind ca sheet with
    | None -> S "" // We define an empty cell to be the empty string value.
    | Some v -> evalValue v sheet

evalCell (3, 'G') dice

let ppBoard sheet = Map.maxKeyValue ()
