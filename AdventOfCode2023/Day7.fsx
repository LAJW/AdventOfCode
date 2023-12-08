#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System

open System.Text.RegularExpressions
open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let data = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483""".Split("\n")

let data = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483""".Split("\n")

let data = File.ReadAllLines "./AdventOfCode2023/data7.txt"

let parseCard (ch : char) =
    match Int32.TryParse (string ch) with
    | true, value -> value
    | false, _ ->
        match ch with
        | 'T' -> 10
        | 'J' -> 11
        | 'Q' -> 12
        | 'K' -> 13
        | 'A' -> 14
        | _ -> failwith ("Unknown card: " + string ch)

type CardSet =
    | FiveOfAKind of int
    | FourOfAKind of int
    | FullHouse of int * int
    | ThreeOfAKind of int
    | TwoPair of int * int
    | Pair of int
    | HighCard of int

let parseSet (cards : int list) =
    match cards |> List.groupBy id |> map (fun (card, group) -> card, group.Length) |> sortByDescending fst |> sortByDescending snd with
    | [(x, 5)] -> FiveOfAKind x
    | [(x, 4); _] -> FourOfAKind x
    | [(x, 3); (y, 2)] -> FullHouse(x, y)
    | (x, 3) :: _ -> ThreeOfAKind x
    | (x, 2) :: (y, 2) :: _ -> TwoPair (x, y)
    | (x, 2) :: _ -> Pair x
    | (x, 1) :: _ -> HighCard x
    | x -> failwith ("Unreachable" + x.ToString())

let superParseSet (cards : int list) =
    let nonJokers = cards |> List.filter ((<>) 11)
    let jokerValue =
        match nonJokers with
        | [] -> 14
        | nonJokers ->
            nonJokers |> List.groupBy id |> List.sortBy fst |> List.maxBy (snd >> List.length) |> fst
    
    match cards |> List.replace [11] [jokerValue] |> List.groupBy id |> map (fun (card, group) -> card, group.Length) |> sortByDescending fst |> sortByDescending snd with
    | [(x, 5)] -> FiveOfAKind x
    | [(x, 4); _] -> FourOfAKind x
    | [(x, 3); (y, 2)] -> FullHouse(x, y)
    | (x, 3) :: _ -> ThreeOfAKind x
    | (x, 2) :: (y, 2) :: _ -> TwoPair (x, y)
    | (x, 2) :: _ -> Pair x
    | (x, 1) :: _ -> HighCard x
    | x -> failwith ("Unreachable" + x.ToString())

data |> Seq.map(fun row ->
    let [|cards; money|] = row.Split " "
    let parsed = cards |> Seq.map (parseCard) |> toList
    parseSet parsed, parsed, Int32.Parse money
)
|> Seq.sortWith(fun a b ->
    let (setA, listA, _) = a
    let (setB, listB, _) = b
    let main =
        match setA, setB with
        | FiveOfAKind a, FiveOfAKind b -> a.CompareTo(b)
        | FiveOfAKind a, _ -> 1
        | _, FiveOfAKind a -> -1
        | FourOfAKind a, FourOfAKind b -> a.CompareTo(b)
        | FourOfAKind a, _ -> 1
        | _, FourOfAKind a -> -1
        | FullHouse(x, y), FullHouse(xb, yb) ->
            let first = x.CompareTo(xb)
            if first = 0 then y.CompareTo(yb) else first
        | FullHouse _, _ -> 1
        | _, FullHouse _ -> -1
        | ThreeOfAKind a, ThreeOfAKind b -> a.CompareTo(b)
        | ThreeOfAKind _, _ -> 1
        | _, ThreeOfAKind _ -> -1
        | TwoPair(x, y), TwoPair(xb, yb) ->
            let first = x.CompareTo(xb)
            if first = 0 then y.CompareTo(yb) else first
        | TwoPair _, _ -> 1
        | _, TwoPair _ -> -1
        | Pair a, Pair b -> a.CompareTo(b)
        | Pair _, _ -> 1
        | _, Pair _ -> -1
        | HighCard a, HighCard b -> a.CompareTo(b)
    if main <> 0 then main
    else Seq.zip listA listB |> map (fun (a, b) -> a.CompareTo(b)) |> tryFind ((<>) 0) |> Option.defaultValue 0
)
|> Seq.zip (Seq.initInfinite ((+) 1))
|> Seq.map (fun (rank, (_, _, money)) -> rank * money)
|> Seq.sum

data |> Seq.map(fun row ->
    let [|cards; money|] = row.Split " "
    let parsed = cards |> Seq.map (parseCard) |> toList
    superParseSet parsed, (parsed |> List.replace [11] [1]), Int32.Parse money
)
|> Seq.sortWith(fun a b ->
    let (setA, listA, _) = a
    let (setB, listB, _) = b
    let main =
        match setA, setB with
        | FiveOfAKind a, FiveOfAKind b -> 0
        | FiveOfAKind a, _ -> 1
        | _, FiveOfAKind a -> -1
        | FourOfAKind a, FourOfAKind b -> 0
        | FourOfAKind a, _ -> 1
        | _, FourOfAKind a -> -1
        | FullHouse(x, y), FullHouse(xb, yb) -> 0
        | FullHouse _, _ -> 1
        | _, FullHouse _ -> -1
        | ThreeOfAKind a, ThreeOfAKind b -> 0
        | ThreeOfAKind _, _ -> 1
        | _, ThreeOfAKind _ -> -1
        | TwoPair(x, y), TwoPair(xb, yb) -> 0
        | TwoPair _, _ -> 1
        | _, TwoPair _ -> -1
        | Pair a, Pair b -> 0
        | Pair _, _ -> 1
        | _, Pair _ -> -1
        | HighCard a, HighCard b -> 0
    if main <> 0 then main
    else Seq.zip listA listB |> map (fun (a, b) -> a.CompareTo(b)) |> tryFind ((<>) 0) |> Option.defaultValue 0
)
|> Seq.zip (Seq.initInfinite ((+) 1))
|> Seq.map (fun (rank, (_, _, money)) -> rank * money)
|> Seq.sum
|> printfn "Part 2: %d"
