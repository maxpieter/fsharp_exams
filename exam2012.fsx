type Name = string
type Score = int
type Result = Name * Score

let testResults: List<Result> =
    [ ("Alice", 95)
      ("Bob", 67)
      ("Charlie", 88)
      ("Dana", 42)
      ("Eve", 100)
      ("Frank", 76)
      ("Grace", 59) ]

// 1.1
let rec legalResults (res: List<Result>) =
    match res with
    | [] -> true
    | (_, score) :: xs -> 0 <= score && score <= 100 && legalResults xs


// 1.2 find the maximum
let maxScore (res: List<Result>) =
    match res with
    | [] -> failwith "we don't care"
    | (_, score) :: xs ->
        let rec aux rs acc =
            match rs with
            | [] -> acc
            | (_, s) :: tail ->
                let newAcc = if s > acc then s else acc
                aux tail newAcc

        aux xs score

let maxScoreGPT res =
    match res with
    | [] -> failwith "we don't care"
    | (_, score) :: xs -> List.fold (fun acc (_, s) -> max acc s) score xs

// 1.3
let rec best res =
    let bestScore = maxScore res

    match res with
    | [] -> failwith "Empty list"
    | (name, score) :: xs -> if score = bestScore then (name, score) else best xs

// 1.4 find the average
let average (res: List<Result>) =
    match res with
    | [] -> failwith "Empty list"
    | (_, score) :: xs ->
        let total = List.fold (fun acc (_, s) -> s + acc) score xs
        float total / float (List.length res)

// 1.5 Delete item from list
let delete (r: Result) res = List.filter (fun x -> x <> r) res

// 1.6 Select n items from list
exception InvalidInputException of string

let rec bestN res n =
    match n, res with
    | 0, _ -> []
    | _, [] -> raise (InvalidInputException "n is larger than list size")
    | _ ->
        let top = best res
        let remaining = delete top res
        top :: bestN remaining (n - 1)

legalResults testResults
maxScore testResults
best testResults
average testResults
let results = bestN testResults 3

// 2.1
type Typ =
    | Integer
    | Boolean
    | Ft of Typ list * Typ

type Decl = string * Typ

let rec distinctVars list =
    match list with
    | [] -> true
    | (name, _) :: xs -> not (List.exists (fun (n, _) -> n = name) xs) && distinctVars xs

let distinctVarsGPT list =
    let names = List.map fst list
    let uniqueNames = Set.ofList names
    List.length names = Set.count uniqueNames





// 3.1
let rec h a b =
    match a with
    | [] -> b
    | c :: d -> c :: h d b

h
// Function h concatenates two lists, adding the elements of the first list in front of the second list, preserving their order.

// 3.2
type T<'a, 'b> =
    | A of 'a
    | B of 'b
    | C of T<'a, 'b> * T<'a, 'b>

let example1 = C(A 10, C(B true, A 7))

// 3.3
let example2: T<obj list, obj option> = C(A [], C(B(Some false), A []))

let rec f1 =
    function
    | C(t1, t2) -> 1 + max (f1 t1) (f1 t2)
    | _ -> 1
// takes a parameter of type C,
// -> if the type is a tuple of type T, recursively calls itself on both elements in the tuple, then finds the maxiumum element of the two and adds that to 1
// -> if the type is anything else (so A of 'a or B of 'b) return 1

// f1 computes the height of a tree defined by the type T<'a, 'b>, where A and B are treated as leaves,
// and C(t1, t2) is treated as an internal node with two subtrees.

let rec f2 =
    function
    | A e
    | B e -> [ e ] // If the node is A e or B e, return a singleton list containing e
    | C(t1, t2) -> f2 t1 @ f2 t2 // If the node is C(t1, t2), recursively flatten both subtrees and concatenate their results

// This function builds a flat list of all values stored in the tree’s leaves, in left-to-right order.

let rec f3 e b t = // 'a -> bool -> T<'a, 'a> -> T<'a, 'a>
    match t with
    | C(t1, t2) when b -> C(f3 e b t1, t2) // If b is true and t is of type C -> recursively call f3 on the first elem in the tuple
    | C(t1, t2) -> C(t1, f3 e b t2) // If be is not true -> recursively call f3 on the second elem in the tuple
    | _ when b -> C(A e, t) // when b is true, return a new type C with (A e, T)
    | _ -> C(t, B e) // otherwise, return a C with (t, B e)

// f3 inserts a value e into a tree of type T<'a, 'a>, navigating down the tree according to a boolean flag b.
// 	•	If b = true, it goes down the left branches.
// 	•	If b = false, it goes down the right branches.
// When a leaf is reached, it wraps the tree and e in a new C node, placing e on the appropriate side.
