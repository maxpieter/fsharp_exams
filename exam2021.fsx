type Rope =
    | Leaf of string * int
    | Node of Rope * int * Rope

let rope1 =
    Node(Node(Leaf("I_lik", 5), 5, Leaf("e_functional_pr", 15)), 20, Node(Leaf("ogr", 3), 3, Leaf("amming", 6)))

// 1.1
let rope2 = Node(Leaf("_and", 4), 4, Node(Leaf("_very_", 6), 6, Leaf("much_F#", 7)))

// Rope is a monomorphic type since the type of values that can be used are restricted to string, Rope (recursive) and int

let rope3 =
    Node(Node(Leaf("example_", 8), 8, Leaf("with_", 5)), 13, Leaf("5_nodes", 7))

// 1.2
let rec length (r: Rope) =
    match r with
    | Leaf(_, int) -> int
    | Node(left, _, right) -> length left + length right
// recursive function that continuously calls length on Nodes
// once a Leaf is encountered, the recursion terminates and returns the integer in the leaf
length rope1

let rec flatten r =
    match r with
    | Leaf(str, _) -> str
    | Node(left, _, right) -> flatten left + flatten right
// recursive function that continuously calls flatten on Nodes
// once a Leaf is encountered, the recursion terminates and returns the string in the leaf
flatten rope1

let index i r =
    let str = flatten r

    match i with
    | _ when i >= str.Length -> failwith "index out of bounds"
    | _ when i < str.Length -> str.[i]
// first the flatten function is used to create one string
// then a quick check is done via pattern matching to ensure the index in within bounds
// if out of bounds, it fails with index out of bounds, if within, it returns the i'th index of the string (0-indexed)
index 2 rope1

// 1.3
let concat r1 r2 = Node(r1, length r1, r2)

let concatRes = concat rope1 rope2
fsi.PrintSize <- 10000

let rec prettyPrint r =
    match r with
    | Leaf(str, i) -> sprintf "\t\tLeaf(\"%s\",  %d)" str i
    | Node(left, i, right) -> sprintf "\t Node( \n\t %s, \n\t\t %d, \n\t %s)" (prettyPrint left) i (prettyPrint right)

printfn "%s" (prettyPrint rope1)

// 2.1



// 4.1
type stack = int list

type inst =
    | ADD
    | SUB
    | PUSH of int
    | LABEL of string
    | IFNZGOTO of string
    | EXIT

let insts01 = [ PUSH 10; PUSH 12; ADD; EXIT ]
let insts02 = [ PUSH 10; LABEL "sub1"; PUSH 1; SUB; IFNZGOTO "sub1"; EXIT ]

let execInsts insts =
    let rec exec insts s =
        match insts, s with
        | SUB :: is, v1 :: v2 :: s -> exec is (v2 - v1 :: s)
        | ADD :: is, v1 :: v2 :: s -> exec is (v1 + v2 :: s)
        | PUSH x :: is, s -> exec is (x :: s)
        | LABEL lab :: _, s -> failwith "LABEL not implemented"
        | IFNZGOTO lab :: _, s -> failwith "IFNZGOTO not implemented"
        | EXIT :: is, s -> List.sum s
        | _ -> failwith "Missing stack values for instruction"

    exec insts []
// The pattern matching selects the first item from the list of instruction and matches agains the cases defined.
// for SUB and ADD, the first two element from the stack are selected to perform the operator on
// for the PUSH operator. the integer associated with push is also collected from the list of instructions
// then the integer is pushed onto the stack
// Hereafter, the exec helper function is recursively called with the new list of instructions and stack


// 4.2
type resolvedInst =
    | RADD
    | RSUB
    | RPUSH of int
    | RIFNZGOTO of int
    | REXIT

type prog = Map<resolvedInst, int> // switched them around f

let buildEnv insts =
    let rec build idx env =
        function
        | [] -> env
        | LABEL lab :: insts -> build idx (Map.add lab idx env) insts // add entry into map
        | _ :: insts -> build (idx + 1) env insts

    build 0 Map.empty insts

buildEnv insts02

// 4.3
type env = Map<string, int>

let lookup l m =
    match Map.tryFind l m with
    | None -> failwith "Value not in map"
    | Some v -> v

let resolveInsts insts env =
    let rec resolve idx =
        function
        | [] -> Map.empty
        | LABEL lab :: insts -> resolve idx insts
        | ADD :: insts -> Map.add idx RADD (resolve (idx + 1) insts)
        | SUB :: insts -> Map.add idx RSUB (resolve (idx + 1) insts)
        | PUSH v :: insts -> Map.add idx (RPUSH v) (resolve (idx + 1) insts)
        | IFNZGOTO lab :: insts -> Map.add idx (RIFNZGOTO(lookup lab env)) (resolve (idx + 1) insts)
        | EXIT :: insts -> Map.add idx REXIT (resolve (idx + 1) insts)

    resolve 0 insts


resolveInsts insts02 (buildEnv insts02)
