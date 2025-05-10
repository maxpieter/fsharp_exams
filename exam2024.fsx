// 1.1
let noRemainderP m n = m % n = 0
// m % n = 0 is a boolean expression
noRemainderP 10 2

//
let checkNubmer n m =
    List.forall (fun i -> noRemainderP m i) [ 1..n ]
// .NET method to implement the function
let checkNumberT n m =
    let rec loop lst bool =
        match lst with
        | [] -> bool
        | x :: xs -> loop xs (bool && noRemainderP m x)

    loop [ 1..n ] true

checkNumberT 10 2520

//
let untilTrue f =
    let rec loop lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> if f x = true then x else loop xs 0

    loop [ 1..10000000 ] 0

let untilTrue' f =
    let rec loop x = if f x then x else loop (x + 1)

    loop 1

//
let findSmallest n = n |> checkNubmer |> untilTrue'

findSmallest 3

// 1.2
let rec revAppend xs ys =
    match xs with
    | [] -> ys
    | x :: xs -> revAppend xs (x :: ys)

revAppend [ 1; 2 ] [ 3; 4 ]

//
let average xs =
    match xs with
    | [] -> 0.0
    | _ ->
        let rec loop lst acc n =
            match lst with
            | [] -> acc / float n
            | x :: xs' -> loop xs' (acc + x) (n + 1)

        loop xs 0 0

average [ 1.0; 2.0 ]
average []

//
let maxBy f xs =
    match xs with
    | [] -> failwith "maxBy: empty list"
    | x :: xs ->
        let rec loop lst max =
            match lst with
            | [] -> max
            | y :: xs -> if f y > f max then loop xs y else loop xs x

        loop xs x

maxBy ((+) 1) [ 1..10 ]

// 1.3
let rec collect f =
    function
    | [] -> []
    | x :: xs -> f x @ collect f xs

// it is not tail recursive because the concatination waits until the recurive calls on collect are completed

//
let collectT f xs =
    let rec loop lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> loop xs (f x @ acc)

    loop xs []

collect id [ [ 1; 2 ]; [ 3; 4 ] ]

//2.1
type 'a diffList =
    | LISTS of 'a list list
    | SEQ of 'a diffList * 'a diffList

let exDF01 =
    SEQ(LISTS [ [ 1; 2 ]; [ 3; 4 ] ], SEQ(LISTS [ []; [ 5 ] ], LISTS [ [ 6; 7 ]; [ 8 ] ]))

//
let exDF02 = LISTS [ [ 1; 2; 3; 4 ]; [ 5; 6; 7; 8 ] ]

exDF01 = exDF01 // true
exDF01 = exDF02 // false

//
let mkDiffList = LISTS[[]]

//
let fromList xs = LISTS[xs]

//
let appendList xs diffList =
    match diffList with
    | LISTS [ t ] -> LISTS [ t; xs ]
    | _ -> failwith "list can't be empty"

appendList [ 1; 2 ] (fromList [ 3; 4 ]) // LISTS [[3; 4]; [1; 2]]

//
let append diffXs diffYs =
    match diffXs, diffYs with
    | LISTS [ xs ], LISTS [ ys ] -> LISTS [ xs; ys ]
    | _ -> failwith "list can't be empty"

append (fromList [ 1; 2 ]) (fromList [ 3; 4 ])

// 2.1
let rec flatten diffXs =
    match diffXs with
    | LISTS xs -> collect id xs
    | SEQ(x, y) -> flatten x @ flatten y
    | _ -> failwith "list can't be empty"

flatten exDF01 // [1; 2; 3; 4; 5; 6; 7; 8]

//
// let map f diffXs = diffXs |> flatten |> List.map f

let rec map f diffXs =
    match diffXs with
    | LISTS xs ->
        let rec loop lst acc =
            match lst with
            | x :: xs ->
                let x' = List.map f x
                loop xs (acc @ [ x' ])
            | [] -> LISTS acc

        loop xs []
    | SEQ(x, y) -> SEQ(map f x, map f y)

flatten (map ((+) 1) exDF01)

//3.1
type Currency =
    | DKK
    | EUR
    | USD

type TransactionType =
    | Deposit
    | Withdrawal

type Transaction =
    { date: int * int * int
      transType: TransactionType
      currency: Currency
      amount: float }

type Account =
    { number: string
      owner: string
      transactions: Transaction list }

let exTransData01 =
    [ (Deposit, DKK, 45.5, (1, 1, 2024))
      (Withdrawal, DKK, 90.0, (1, 2, 2024))
      (Deposit, USD, 10.0, (14, 2, 2024))
      (Withdrawal, USD, 5.0, (14, 1, 2024))
      (Deposit, EUR, 42.0, (16, 2, 2024)) ]

//
let mkTrans (transType, currency, amount, (day, month, year)) =
    { date = day, month, year
      transType = transType
      currency = currency
      amount = amount }

//
let exTrans01 = List.map mkTrans exTransData01

let createAcc accNum owner transactions =
    { number = accNum
      owner = owner
      transactions = transactions }

let exAccount01 = createAcc "001" "Hans" exTrans01

//
let addTransToAcc account transaction =
    match account with
    | { number = n
        owner = o
        transactions = xs } ->
        { number = n
          owner = o
          transactions = transaction :: xs }

let exAccount02 =
    addTransToAcc exAccount01 (mkTrans (Deposit, USD, 10.0, (3, 2, 2024)))

//
let deposit account currency (day, month, year) amount =
    mkTrans (Deposit, currency, amount, (day, month, year)) |> addTransToAcc account

deposit exAccount01 DKK (13, 2, 2024) 42.1

//3.2
type Balances = Map<Currency, float>

let getBalance currency balances =
    match Map.tryFind currency balances with
    | None -> 0.0
    | Some s -> s

getBalance DKK (Map [ (DKK, 42.0); (USD, 43.25) ])
getBalance DKK (Map [])

//
let addToBalance balances (currency, transType, amount: float) =
    match Map.tryFind currency balances with
    | Some s ->
        if transType = Deposit then
            Map.add currency (s + amount) balances
        else
            Map.add currency (s - amount) balances
    | None ->
        if transType = Deposit then
            Map.add currency amount balances
        else
            Map.add currency (-amount) balances

addToBalance Map.empty (DKK, Deposit, 100.0)
addToBalance Map.empty (USD, Withdrawal, 100.0)

//
let getBalancesOfAccount account =
    match account with
    | { number = _
        owner = _
        transactions = transactions } ->
        let rec loop lst acc =
            match lst with
            | { date = _
                transType = tt
                currency = c
                amount = a } :: xs -> loop xs (addToBalance acc (c, tt, a))
            | [] -> acc

        loop transactions Map.empty

getBalancesOfAccount exAccount01

// 3.3
let conversionRates =
    Map
        [ (USD, Map [ (EUR, 0.93); (DKK, 6.94) ])
          (EUR, Map [ (USD, 1.07); (DKK, 7.46) ])
          (DKK, Map [ (USD, 0.14); (EUR, 0.13) ]) ]

let getConvRate fromCurrency toCurrency (rates: Map<'a, Map<'b, float>>) =
    match Map.tryFind fromCurrency rates with
    | None -> failwith $"We don't exchange {fromCurrency}"
    | Some m ->
        match Map.tryFind toCurrency m with
        | None -> failwith $"{fromCurrency} does not convert to {toCurrency}"
        | Some m -> m

getConvRate USD DKK conversionRates

let convertFunds account fromCurrency toCurrency rates =
    let convRate = getConvRate fromCurrency toCurrency rates

    let newTransactions =
        account.transactions
        |> List.map (fun t ->
            if t.currency = fromCurrency then
                { t with
                    currency = toCurrency
                    amount = t.amount * convRate }
            else
                t)

    { account with
        transactions = newTransactions }


let exAccount03 = convertFunds exAccount01 USD EUR conversionRates

getBalancesOfAccount exAccount01
getBalancesOfAccount exAccount03


// 4.1
let fizzBuzz n =
    if n % 3 = 0 && n % 5 = 0 then "FizzBuzz"
    elif n % 3 = 0 then "Fizz"
    elif n % 5 = 0 then "Buzz"
    else $"{n}"

fizzBuzz 55
fizzBuzz 7

//
let fizzBuzzSeq = Seq.initInfinite fizzBuzz

// extract index from sequence
let fizzBuzzSeq2 = Seq.mapi (fun i v -> i, v) fizzBuzzSeq
