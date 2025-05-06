// 1.1

type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>

let psEx = PrioritySet [ "a"; "b"; "c" ]

let priSetEx = PrioritySet [ "a"; "q"; "b"; "d" ]

// the type is of PrioritySet<string>

let empty = PrioritySet List.empty

// 1.2

let isEmpty (PrioritySet ps) =
    match ps with
    | [] -> true
    | _ -> false


isEmpty empty
isEmpty priSetEx

let size (PrioritySet ps) = List.length ps

size empty
size priSetEx

let contains e (PrioritySet ps) = List.contains e ps

contains "g" priSetEx

let getPN e (PrioritySet ps) =
    if contains e (PrioritySet ps) then // define the type by adding it before
        List.findIndex ((=) e) ps + 1 // add function (=) to the findIndex function
    else
        failwith "element not in PrioritySet"

getPN "a" priSetEx

// 1.3
let remove e (PrioritySet ps) : PrioritySet<'a> =
    PrioritySet(List.removeAt (List.findIndex ((=) e) ps) ps)

remove "a" priSetEx

let add e (PrioritySet ps) =
    if contains e (PrioritySet ps) then
        PrioritySet(ps)
    else
        PrioritySet(ps @ [ e ])

add "z" priSetEx

// 1.4
let map f (PrioritySet ps) : PrioritySet<'a> = PrioritySet(List.map f ps)

map (fun (c: string) -> c.ToUpper()) psEx

let cp (PrioritySet ps1: PrioritySet<'a>) (PrioritySet ps2: PrioritySet<'b>) : PrioritySet<'a * 'b> =
    if List.isEmpty ps1 || List.isEmpty ps2 then
        PrioritySet []
    else // when construcuting cartesian product, you can run a nested for-loop locked into a list
        let pairs =
            [ for x in ps1 do
                  for y in ps2 do
                      yield x, y ]

        PrioritySet pairs

// 2.1
let f (curRow) =
    let rec f' =
        function
        | [] -> []
        | [ _ ] -> [ 1 ]
        | xs ->
            let x1 :: x2 :: xs = xs
            x1 + x2 :: f' (x2 :: xs)

    1 :: f' curRow

f [ 1 ]

// it takes a current row as input and computes the next row for Pascal’s triangle.
// This row always starts and ends with 1 and is of size n + 1 where n = size of input row

let fMatch curRow =
    let rec fMatch' =
        function
        | [] -> []
        | [ _ ] -> [ 1 ]
        | x1 :: x2 :: xs -> x1 + x2 :: fMatch' (x2 :: xs)

    1 :: fMatch' curRow
// the warning disappears because the

let fTail curRow =
    let rec fTail' acc =
        function
        | [] -> acc
        | [ _ ] -> 1 :: acc
        | x1 :: x2 :: xs -> fTail' ((x1 + x2) :: acc) (x2 :: xs) // string concatination

    fTail' [ 1 ] curRow

fTail [ 1; 4; 6; 4; 1 ]

// 3.1
let mySeq s1 s2 =
    seq {
        for e1 in s1 do
            for e2 in s2 do
                yield! [ e1; e2 ]
    }
// this function returns a list of all the elements in all cartesian products of two sequence. Each

let seq1 =
    seq {
        'A'
        'B'
    }

let seq2 =
    seq {
        'D'
        'E'
        'F'
    }

// 3.2
let mySeq2 (s1: seq<'a>) (s2: seq<'b>) : seq<'a * 'b> =
    [ for x in s1 do
          for y in s2 do
              yield x, y ]

mySeq2 seq1 seq2 // returns the cartesian product but now paired into groups

// 3.3
let mySeq3 n =
    Seq.initInfinite (fun i -> float n ** 2.0 - float n * float i)

mySeq3 5


// 4.1
type DataSpec =
    | RangeInt of int * int
    | ChoiceString of string list
    | StringSeq of string
    | Pair of DataSpec * DataSpec
    | Repeat of int * DataSpec
    | Pick of string
    | Label of string * DataSpec

let reg =
    [ ("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a3", ("soft drink", 5)) ]

let regDS =
    Repeat(
        3,
        Pair(
            StringSeq "a",
            Pair(
                ChoiceString["cheese"
                             "herring"
                             "soft drink"],
                RangeInt(1, 100)
            )
        )
    )


let pur = [ 3, "a2"; 1, "a1" ]

let purDS = Repeat(2, Pair(RangeInt(1, 10), StringSeq "a"))

// 4.2
let rand = System.Random()
let next (i1, i2) = rand.Next(i1, i2)

let numGen =
    let n = ref 0

    fun () ->
        n := !n + 1
        !n

// let rec genValue =
//     function
//     | RangeInt(i1, i2) -> next(i1, i2).ToString()
//     | ChoiceString xs -> xs.[next (0, List.length xs - 1)]
//     | StringSeq a -> a + numGen().ToString()
//     | Pair(ds1, ds2) -> $"({genValue ds1}, {genValue ds2})" // include variable into string literal
//     | Repeat(n, ds) -> "[" + String.concat ";" (Seq.init n (fun _ -> genValue ds)) + "]"


// genValue regDS

// 4.3
let reg2 =
    Repeat(
        3,
        Pair(
            Label("articleCode", StringSeq "a"),
            Pair(
                ChoiceString["cheese"
                             "herring"
                             "soft drink"],
                RangeInt(1, 100)
            )
        )
    )

let pur2 = Repeat(2, Pair(RangeInt(1, 10), Pick "articleCode"))

type Env = Map<string, string list>
let env = Env [ ("x", [ "43"; "55"; "88" ]) ]

let addToEnv (s: string) (v: string) (dEnv: Env) : Env =
    match Map.tryFind s dEnv with
    | Some lst -> Map.add s (v :: lst) dEnv
    | None -> Map.add s [ v ] dEnv

addToEnv "x" "42" env

let pickFromEnv s dEnv =
    match Map.tryFind s dEnv with
    | Some(lst: list<string>) -> lst.[next (0, List.length lst - 1)]
    | None -> failwith "Not in environment"

pickFromEnv "x" env
let value s dEnv = Map.tryFind s dEnv

value "x" env

let rec genValue dEnv =
    function
    | RangeInt(i1, i2) -> next(i1, i2 + 1).ToString(), dEnv
    | ChoiceString xs ->
        let idx = next (0, List.length xs)
        List.item idx xs, dEnv // get element from list
    | StringSeq s -> s + numGen().ToString(), dEnv
    | Pair(ds1, ds2) ->
        let v1', dEnv1 = genValue dEnv ds1
        let v2', dEnv2 = genValue dEnv1 ds2
        "(" + v1' + "," + v2' + ")", dEnv2
    | Repeat(n, ds) ->
        let rec iter (dEnv, vs) =
            function
            | 0 -> dEnv, List.rev vs
            | n when n > 0 ->
                let v', dEnv' = genValue dEnv ds
                iter (dEnv', v' :: vs) (n - 1)
            | _ -> failwith "Repeat with n < 0"

        let dEnv', vs' = iter (dEnv, []) n
        "[" + String.concat ";" vs' + "]", dEnv'
    | Pick s -> pickFromEnv s dEnv, dEnv
    | Label(s, ds) ->
        let v', dEnv' = genValue dEnv ds
        v', addToEnv s v' dEnv'

let v, dEnv = genValue Map.empty reg2
genValue dEnv pur2
