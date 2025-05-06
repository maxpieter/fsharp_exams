// 1.1

type Multiset<'a when 'a: equality> = ('a * int) list

let rec inv (ms: Multiset<'a>) : bool =
    match ms with
    | [] -> true
    | (elem, multiplicity) :: xs -> multiplicity > 0 && not (List.exists (fun (e, _) -> e = elem) xs) && inv xs

printfn "%b" (inv [ ("apple", 2); ("banana", 3) ])
printfn "%b" (inv [ (1, 1); (2, 2); (3, 3) ])
printfn "%b" (inv [ ("a", 1) ])
printfn "%b" (inv [])

printfn "%b" (inv [ ("apple", 0) ]) // false: multiplicity is zero
printfn "%b" (inv [ ("apple", -1) ]) // false: negative multiplicity
printfn "%b" (inv [ ("apple", 2); ("apple", 3) ]) // false: duplicate element
printfn "%b" (inv [ (1, 1); (2, 0) ]) // false: second element has 0 multiplicity
printfn "%b" (inv [ ("a", 1); ("b", 2); ("a", 1) ]) // false: "a" appears twice


// 1.2
// find a string key in a list of (string * int) and add n to int if exists, otherwise, append to list

let insert (e: 'a) (n: int) (ms: Multiset<'a>) : Multiset<'a> =
    ms
    |> List.map (fun (value, amount) -> if value = e then (value, amount + n) else (value, amount))
    |> fun updatedMs ->
        if List.exists (fun (value, _) -> value = e) ms then
            updatedMs
        else
            (e, n) :: ms

// 1.3
// returns the number of occurences of key e with tryFind and some / none
let numberOf e (ms: Multiset<'a>) =
    match List.tryFind (fun (value, _) -> value = e) ms with
    | Some(_, amount) -> amount
    | None -> 0

// 1.4

exception EmptyInput of string

let delete e (ms: Multiset<'a>) : Multiset<'a> =
    ms
    |> List.map (fun (value, amount) -> if value = e then (value, amount - 1) else value, amount)
    |> fun updatedMs ->
        if List.exists (fun (value, _) -> value = e) ms then
            updatedMs
        else
            raise (EmptyInput "input cannot be empty")

let rec union ms1 ms2 =
    match (fun ()) with
    | [], _ -> ms2
    | _, [] -> ms1
    | _, _ -> union ms1,
