//1.1
type multimap<'a, 'b when 'a: comparison> = MMap of Map<'a, list<'b>>

let ex = MMap(Map.ofList [ ("record", [ 50 ]); ("ordering", [ 36; 46; 70 ]) ])

let studReg =
    MMap(
        Map.ofList
            [ ("Grete", [])
              ("Hans", [ "TOPS"; "HOPS" ])
              ("Peter", [ "IFFY" ])
              ("Sine", [ "HOPS"; "IFFY"; "BFNP" ]) ]
    )

//
let studReg2 =
    MMap(
        Map.ofList
            [ ("Grete", [])
              ("Hans", [ "TOPS"; "HOPS" ])
              ("Peter", [ "IFFY" ])
              ("Sine", [ "HOPS"; "IFFY"; "BFNP" ]) ]
    )

studReg = studReg2
// changeing the order of the courses makes them not the same for the compiler

//1.2
let canonical (MMap m) = //unwrap the type
    let m' = Map.toList m

    let rec loop lst acc =
        match lst with
        | [] -> MMap(Map.ofList acc)
        | (v, toSort) :: xs -> loop xs ((v, (List.sort toSort)) :: acc)

    loop m' []

canonical studReg

//
let toOrderedList (MMap m) =
    let m' = Map.toList m

    let rec loop lst acc =
        match lst with
        | [] -> acc
        | (v, toSort) :: xs -> loop xs ((v, (List.sort toSort)) :: acc)

    loop m' []

canonical studReg

let studRegOrdered = toOrderedList studReg

//1.3
let newMultimap = MMap(Map.empty)

//
let sizeMultimap (MMap m) =
    let m' = Map.toList m

    let rec loop lst (kc, vc) =
        match lst with
        | [] -> (kc, vc)
        | (k, v) :: xs -> loop xs (kc + 1, vc + List.length v)

    loop m' (0, 0)

sizeMultimap studReg

// 1.4
let addMultimap k (v: string) (MMap m) =
    match Map.tryFind k m with
    | None -> MMap(Map.add k [ v ] m)
    | Some s ->
        match s with
        | [] -> MMap(Map.add k [ v ] m)
        | xs ->
            if List.contains (string v) xs then
                MMap m
            else
                MMap(Map.add k (v :: xs) m)

addMultimap "Sine" "BFNP" studReg
sizeMultimap (addMultimap "Grete" "TIPS" studReg) = (4, 7)
sizeMultimap (addMultimap "Pia" "" studReg) = (5, 7)


// 1.5
let mapMultimap f (MMap m: multimap<'a, 'b>) =
    MMap(Map.map (fun k v -> List.map (f k) v) m)

mapMultimap (fun k v -> v + "-F2015") studReg

// 1.6 // implement fold over a map (with nested list)
let foldMultimap f s (MMap m) =
    Map.fold (fun acc k vs -> List.fold (fun acc' v -> f acc' k v) acc vs) s m

foldMultimap (fun acc k v -> String.length v + acc) 0 studReg

// 2.0
let rec f i j xs =
    if xs = [] then
        [ i * j ]
    else
        let x :: xs' = xs
        x * i :: f (i * j) (-1 * j) xs'

f 10 1 [ 1..9 ]

// f evaluates to a list of size of xs + 1 with i being multiplied by the ith element in the list xs
// alternating between two positive multiplications and two negative multiplications. (carried over by the two parameters being both positive, both negative of a mix of pos/neg )
// The list ends with the latest value of j (positive 1 for odd sized lists, negative for even sized lists) multiplied by i (always 10/-10)
//

// in the if then else clause, the else clause does not have access to the if clause. This means that the compiler warns us that ( let x::xs' = xs ) could result in an error
// because xs could be empty. We ofcourse know that that can't happen since it would be caught by the if cause

//
let rec fMatch i j xs =
    match xs with
    | [] -> [ i * j ]
    | x :: xs' -> x * i :: fMatch (i * j) (-1 * j) xs'

fMatch 10 1 [ 1..9 ]

// 2.2
let fA i j xs =
    let rec loop i j lst acc =
        match lst with
        | [] -> acc @ [ i * j ]
        | x :: lst' -> loop (i * j) (-1 * j) lst' (acc @ [ x * i ])

    loop i j xs []

fA 10 1 [ 1..9 ]


// 3.0
let myFinSeq n m =
    seq {
        for i in [ n..m ] do
            yield [ n..i ]
    }

let res = myFinSeq 5 10 |> Seq.toList

// myFinSeq returns a list with element from i to i up to m, growing the sublist size by 1 per iteration

//
myFinSeq 10 14

// 3 times. from i = 12 through i = 14

// 3.2
let myFinSeq2 n m =
    seq {
        for i in n..m do
            for j in n..i do
                yield j
    }

myFinSeq2 10 14 |> Seq.toList

// 3.3
let sum xs = List.fold (fun r x -> r + x) 0 xs
let seq4000 = myFinSeq 10 4000
let array4000 = Array.ofSeq seq4000

// array4000 has 3991 lists since its an inclusive for-loop (for i = 4000 the loop also runs)
#time
let sums = Array.map sum array4000

#time
#time
// parrallel computation
let psums = Array.Parallel.map sum array4000
#time

// 4.1
type JSONlite = Object of list<string * Value>

and Value =
    | String of string
    | Record of JSONlite
    | Label of string * Value
    | Ref of string

let address = Object [ ("Street", String "Hansedalen"); ("HouseNo", String "27") ]

let person1 =
    Object
        [ ("Name", String "Hans Pedersen")
          ("Address", Label("Addr1", Record address)) ]

let person2 = Object [ ("Name", String "Pia Pedersen"); ("Address", Ref "Addr1") ]
let persons = Object [ ("Person1", Record person1); ("Person2", Record person2) ]

let student =
    Object
        [ ("Name", String "Per Simonsen")
          ("Field", String "BSWU")
          ("Course", Record(Object [ ("BFNP", String "10"); ("BPRD", String "7") ])) ]

let ppJSONlite json =
    let nl = System.Environment.NewLine
    let space n = String.replicate (n * 2) " " // Better readability with 2 spaces per indent
    let ppQuote s = "\"" + s + "\""

    let rec ppValue' indent =
        function
        | String s -> ppQuote s
        | Record js -> ppJSONlite' indent js
        | Label(lbl, v) -> ppValue' indent v // Label is transparent in output
        | Ref s -> ppQuote s

    and ppJSONlite' indent (Object fields) =
        let ind = space indent

        let inner =
            fields
            |> List.map (fun (key, value) -> ind + ppQuote key + " : " + ppValue' (indent + 1) value)
            |> String.concat ("," + nl)

        "{" + nl + inner + nl + space (indent - 1) + "}"

    ppJSONlite' 1 json

ppJSONlite persons

// 4.3
let buildEnv json =
    let rec buildEnvValue env =
        function
        | String _ -> env
        | Ref _ -> env
        | Label(name, v) ->
            let env' = buildEnvValue env v
            Map.add name v env'
        | Record js -> buildEnvJSONlite env js

    and buildEnvJSONlite env =
        function
        | Object xs -> List.fold (fun acc (_, v) -> buildEnvValue acc v) env xs

    buildEnvJSONlite Map.empty json

buildEnv person1

//4.4




let fcont m n cont = m + n |> cont

let gcont m n cont =
    match (m * n) % 2 = 0 with
    | false -> failwith "Odd product"
    | true -> m * n |> cont

gcont 2 4 (fun x -> x + 1)

let add20 x = x + 20

gcont 2 4 add20

let deeper n m cont =
    gcont n m (fun x ->
        let added = x + 10
        let str = string added
        cont str)


let deeperc n m cont =
    gcont n m (fun x -> cont (string (x + 10)))
