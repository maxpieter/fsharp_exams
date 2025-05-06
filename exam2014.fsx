type OrderedList<'a when 'a: equality> = { front: 'a list; rear: 'a list }

let ex = { front = [ 'x' ]; rear = [ 'z'; 'y' ] }

exception OrderedList of string

// 1.1
let ol1 =
    { front = [ "Hans"; "Brian"; "Gudrun" ]
      rear = [] }

let ol2 =
    { front = [ "Hans"; "Brian" ]
      rear = [ "Gudrun" ] }

let ol3 =
    { front = [ "Hans" ]
      rear = [ "Gudrun"; "Brian" ] }

// in total you can represent the list in 4 ways. The above three + a variant where all items are in reversed order in the rear.

// 1.2
let canonical ol : OrderedList<'a> =
    match ol with
    | { front = []; rear = [] } -> { front = []; rear = [] }
    | { front = fr; rear = [] } -> { front = fr; rear = [] }
    | { front = fr; rear = rr } -> { front = fr @ List.rev rr; rear = [] }

let canonical1 ol =
    { front = ol.front @ List.rev ol.rear
      rear = [] }
// lesson: make sure that both have the same returntype

let res = canonical ol3

let toList ol =
    let canonicalList = canonical ol

    match canonicalList with
    | { front = []; rear = _ } -> []
    | { front = fr; rear = _ } ->
        let rec helper fr =
            match fr with
            | [] -> []
            | x :: xs -> [ x ] @ helper xs

        helper fr

// this is not very effcient and uses O(N2) time. You can also call the front of the OL with .front
// Below you can return a list simply by calling the the front section of the list and reverse on the rear.
let toList2 ol = ol.front @ List.rev ol.rear

// 1.3
let newOL () = { front = []; rear = [] }
// a function with type unit -> _ is declared by adding () as input parameter

let isEmpty =
    function
    | { front = []; rear = [] } -> true
    | { front = _; rear = _ } -> false

isEmpty (newOL ())
isEmpty ex

// 1.4
let addFront a ol =
    { front = a :: ol.front
      rear = ol.rear }

let removeFront ol =
    let { front = fr; rear = rr } = canonical1 ol

    match fr with
    | [] -> raise (OrderedList "front is empty")
    | x :: xs -> x, { front = xs; rear = [] }

let a, res3 = removeFront ex

let peekFront ol =

    match ol.front with
    | x :: xs -> x
    | [] ->
        match canonical1 ol with
        | { front = []; rear = _ } -> raise (OrderedList "Ordered list is empty")
        | { front = x :: xs; rear = _ } -> x
// since F# has immutable data structures, returning the first element of the list does not change the original list.
let peekFront1 ol = List.tryHead ol.front

let peek = peekFront ex

// 1.5

let append ol1 ol2 =
    match canonical1 ol1, canonical1 ol2 with
    | { front = fr1; rear = _ }, { front = fr2; rear = _ } -> { front = fr1 @ fr2; rear = [] }

let app = append ol1 ol2

// 1.6

let map (f: 'a -> 'b) ol =
    match canonical1 ol with
    | { front = fr1; rear = _ } -> { front = List.map f fr1; rear = [] }

let map1 f ol =
    { front = List.map f ol.front
      rear = List.map f ol.rear }

let mp = map (fun e -> e.ToString()) ex

// 1.7
let rec fold f acc ol : 'State =
    match toList2 ol with
    | [] -> acc
    | x :: xs -> fold f (f acc x) { front = xs; rear = [] }


fold (fun acc e -> acc + e.ToString()) "" ex

// 1.8
let multiplicity ol =
    let folder acc x =
        match Map.tryFind x acc with
        | Some count -> Map.add x (count + 1) acc
        | None -> Map.add x 1 acc

    fold folder Map.empty ol

let multiplicity1 (ol: OrderedList<'a>) =
    let folder acc x =
        Map.change
            x
            (function
            | None -> Some 1
            | Some n -> Some(n + 1))
            acc

    fold folder Map.empty ol

multiplicity (addFront 'x' ex)

// 2.0
let rec f i =
    function
    | [] -> [ i ]
    | x :: xs -> i + x :: f (i + 1) xs

let rest = f 20 []
printf "%A" rest

// function f takes two parameters. An int argument and then matches on a list. If the list is empty, it returns a list with the int parameter.
// if the list is non-empty, it adds i to each of the items in the list while increasing i after processing each item.
// Finally it reaches the base case and adds the i (then increased by list.length) to the end of the list

// f can never return an empty list. Either it will still be waiting on a parameter (when only provided with a int i)
// Or the i variable will be added to the empty list which results in a non-empty list of size 1

// The function will always terminate when in input list is finite and which will terminate when empty.

let fA i xs =
    let rec loop acc i =
        function
        | [] -> List.rev (i :: acc)
        | x :: xs -> loop ((i + x) :: acc) (i + 1) xs

    loop [] i xs

fA 20 (List.init 100 (fun i -> i))

let fC i xs =
    let rec loop i xs cont =
        match xs with
        | [] -> cont [ i ] //
        | x :: xs -> loop (i + x) xs (fun result -> cont (i + x :: result))

    loop i xs id

fC 20 (List.init 100 (fun i -> i))
// You do not store a list directly.
// Instead, you build a function that remembers:
// “When you give me the final list, I will add my (i+x) value to the front.”
// So every continuation is like a pending operation.


// 3.1
let myFinSeq n M = Seq.map (fun m -> n + n * m) [ 0..M ]

let result = myFinSeq 2 10

printfn "%A" (Seq.toList result)

// the sequence that is returned is M + 1 integers long where each integer is multiplied with n whereafter n is added

// 3.2
let mySeq n = Seq.initInfinite (fun i -> n + n * i)

// 3.3
let multTable N M =
    seq {
        for n in 0..N do
            for m in 0..M do
                yield n, m, n * m
    }

// 3.4



// 4

type opr =
    | MovePenUp
    | MovePenDown
    | TurnEast
    | TurnWest
    | TurnNorth
    | TurnSouth
    | Step

type plot =
    | Opr of opr
    | Seq of plot * plot

let side = Seq(Opr MovePenDown, Seq(Opr Step, Seq(Opr Step, Opr Step)))

let rect =
    Seq(Seq(Opr TurnEast, side), Seq(Opr TurnNorth, Seq(side, Seq(Opr TurnWest, Seq(side, Seq(Opr TurnSouth, side))))))

// 4.1
let ppOpr (opr: opr) = opr.ToString()

ppOpr MovePenUp

let rec ppOprPlot seq =
    match seq with
    | Opr opr -> ppOpr opr
    | Seq(x, y) -> ppOprPlot x + " \n=> " + ppOprPlot y

ppOprPlot rect

// 4.2
type dir =
    | North
    | South
    | West
    | East

type pen =
    | PenUp
    | PenDown

type coord = int * int
type state = coord * dir * pen

let initialState = ((0, 0), East, PenUp)

let goStep ((x, y), dir, pen) : state =
    let newCoord =
        match dir with
        | East -> (x + 1, y)
        | West -> (x - 1, y)
        | North -> (x, y + 1)
        | South -> (x, y - 1)

    newCoord, dir, pen


goStep initialState

let addDot (s: state) (c: coord list) (opr: opr) : coord list * state =
    let newState =
        match opr, s with
        | MovePenUp, ((x, y), d, _) -> (x, y), d, PenUp
        | MovePenDown, ((x, y), d, _) -> (x, y), d, PenDown
        | TurnEast, ((x, y), _, p) -> (x, y), East, p
        | TurnWest, ((x, y), _, p) -> (x, y), West, p
        | TurnNorth, ((x, y), _, p) -> (x, y), North, p
        | TurnSouth, ((x, y), _, p) -> (x, y), South, p
        | Step, ((x, y), d, p) -> goStep ((x, y), d, p)

    // Only add (x,y) if pen is DOWN after the operation
    let coord, _, pen = newState

    if pen = PenDown then coord :: c, newState else c, newState

let (coords1, s1) = addDot initialState [] MovePenDown
let (coords2, s2) = addDot s1 coords1 Step

let rec dotCoordsAux pl coords state =
    match pl with
    | Opr opr -> addDot state coords opr
    | Seq(p1, p2) ->
        let (coords1, state1) = dotCoordsAux p1 coords state
        dotCoordsAux p2 coords1 state1


let dotCoords pl =
    let finalCoords, _ = dotCoordsAux pl [] initialState
    List.rev finalCoords

let coords = dotCoords rect

let coordsSet = Set.ofList coords

// 4.3 operator overloading with type augmentation
type plot with
    static member (+)(p1: plot, p2: plot) : plot = Seq(p1, p2)

let side2 = Opr MovePenDown + Opr Step + Opr Step + Opr Step

let rect2 =
    Opr TurnEast
    + side2
    + Opr TurnNorth
    + side2
    + Opr TurnWest
    + side2
    + Opr TurnSouth
    + side2
