open System
open System.Collections.Generic
open System.IO
open FSharpPlus

(*
let addArrays (a1 : int[]) (a2 : int[]) : int[] =
    Array.zip a1 a2 |> map (fun (a, b) -> a + b)

let join (delim : string) (seq : string seq) =
    String.Join(delim, seq)
    
let binaryToInt (a : string) = Convert.ToInt32(a, 2)

let boolToInt = function true -> 1 | false -> 0
let intToBool x = x <> 0

let arrayToInt raw =
    raw
    |> Seq.map boolToInt
    |> Seq.map (fun x -> x.ToString())
    |> join ""
    |> binaryToInt

let readDigitLines file =
    let allLines = File.ReadAllLines file |> toList
    allLines |> map (fun x -> x.ToCharArray() |> map (string >> Int32.Parse))

let mostCommonBits(lines : int array seq) =
    let length = lines |> length
    lines |> Seq.reduce addArrays |> map (fun count -> count >= length / 2)

let task3_1() =
    let raw = readDigitLines "input3.txt" |> mostCommonBits
    let a = arrayToInt raw
    let b = raw |> Seq.map not |> arrayToInt
    printfn "%d" (a * b)

let mostCommonBits2(lines : int array seq) =
    let length = lines |> length
    let counts = lines |> Seq.reduce addArrays
    // 75875
    lines |> Seq.reduce addArrays |> map (fun count ->
        if count * 2 = length then None
        else Some (count * 2 > length))

let commonStartLength (a : int seq) (b : int option seq) =
    Seq.zip a b
    |> Seq.takeWhile (function
        | _, None -> true
        | a, Some b -> a = b)
    |> Seq.length

let task3_2() =
    let iter (state : int[][]) (index : int) idOrNegate : int[][] =
        if state.Length = 1 then
            state
        else
            let sum = state |> map (fun (row : int[]) -> row.[index]) |> sum
            if sum * 2 = state.Length then
                let ones = true |> idOrNegate
                state |> filter (fun row -> if ones then row.[index] = 1 else row.[index] = 0)
            else
                let ones = sum * 2 > state.Length |> idOrNegate
                state |> filter (fun row -> if ones then row.[index] = 1 else row.[index] = 0)

    let lines = readDigitLines "input3.txt" |> toArray
    let oxygenGeneratorRating =
        seq {0..lines.[0].Length - 1}
        |> fold (fun state index -> iter state index id) lines
        |> head
        |> map intToBool
        |> arrayToInt
    let co2ScrubberRating =
        seq {0..lines.[0].Length - 1}
        |> fold (fun state index -> iter state index not) lines
        |> head
        |> map intToBool
        |> arrayToInt
    printfn "%d" (oxygenGeneratorRating * co2ScrubberRating)

let mark(board : string[][]) number =
    for x in 0..(length board - 1) do
        for y in 0..(length board.[x] - 1) do
            if board.[x].[y] = number then
                board.[x].[y] <- ""
    
let check (board : string[][]) : bool =
    let c1 = board |> exists (forall ((=) ""))
    let c2 =
        seq { 0 .. (length board.[0]) - 1 }
        |> exists (fun x ->
            seq { 0 .. (length board) - 1 }
            |> forall (fun y -> board.[y].[x] = "")
        )
    c1 || c2

let sum(board : string[][]) =
    board |> Seq.concat |> filter ((<>) "") |> map Int32.Parse |> sum

let printBoard (board : string[][]) =
    board |> map (join ",") |> join "\n" |> printfn "%s\n"

let task4_1() =
    let text = File.ReadAllText("input4.txt")
    let chunks = text.Split("\n\n")
    let numbers = chunks.[0].Split(',')
    let boards = chunks |> map (
         String.split ["\n"]
         >> map (String.split [" "] >> filter ((<>) "") >> toArray)
         >> filter (Array.isEmpty >> not)
         >> toArray)
    numbers
    |> Seq.pick(fun number ->
        for board in boards do
            mark board number
        for board in boards do
            printBoard board
        boards |> tryFind check |> map (fun result -> sum result * Int32.Parse(number))
    )

let task4_2() =
    let text = File.ReadAllText("input4.txt")
    let chunks = text.Split("\n\n")
    let numbers = chunks.[0].Split(',')
    let mutable boards = chunks |> map (
         String.split ["\n"]
         >> map (String.split [" "] >> filter ((<>) "") >> toArray)
         >> filter (Array.isEmpty >> not)
         >> toArray)
    let mutable winner = 0
    for number in numbers do
        for board in boards do
            mark board number
        for board in boards do
            printBoard board
        boards |> tryFind check |> map (fun result -> sum result * Int32.Parse(number)) |> iter (fun value -> winner <- value)
        boards <- boards |> filter (not << check)
    winner

let toPair (x : 'T seq) =
    x |> toList |> function
        | [a; b] -> (a, b)
        | _ -> failwith "List should have exactly 2 elements"

let range a b = if a < b then seq {a..b} else seq {b..a}

let task5 diagonalFn =
    File.ReadLines("input5.txt")
        |> Seq.map(
            String.split [" -> "]
            >> map (
                String.split [","]
                >> map Int32.Parse
                >> toPair)
            >> toPair)
        |> Seq.collect (fun ((x1, y1), (x2, y2)) ->
            if x1 = x2
            then range y1 y2 |> map (fun y -> x1, y)
            elif y1 = y2
            then range x1 x2 |> map (fun x -> x, y1)
            else diagonalFn (x1, y1) (x2, y2)
        )
        |> Seq.countBy id
        |> Seq.filter (fun (p, count) -> count > 1)
        |> Seq.length
        |> printfn "%d"

let task5_1() =
    task5 (fun _ _ -> Seq.empty)

let sgn a = if a > 0 then 1 elif a < 0 then -1 else 0

let task5_2() =
    task5 (fun (x1, y1) (x2, y2) ->
        let xsgn = sgn (x2 - x1)
        let ysgn = sgn (y2 - y1)
        let distance = abs(x2 - x1)
        seq {0..distance} |> map (fun t -> x1 + t * xsgn, y1 + t * ysgn)
    )

let task6_1() =
    let numbers = File.ReadLines("input6.txt") |> head |> split [","] |> map Int32.Parse
    seq {1..80}
    |> Seq.fold
       (fun numbers _ -> numbers |> bind (fun number -> if number = 0 then [6; 8] else [number - 1]))
       numbers
    |> Seq.length
    |> printfn "%d"

// optimized 6_1
let task6_2() =
    let state = Array.init 9 (fun _ -> 0L)
    File.ReadLines("input6.txt") |> head |> split [","]
    |> map Int32.Parse
    |> Seq.countBy id
    |> iter (fun (index, count) -> state.[index] <- count)
    for _ in 1..256 do
        let birthCount = state.[0]
        for i in 1..8 do
            state.[i - 1] <- state.[i]
        state.[6] <- state.[6] + birthCount
        state.[8] <- birthCount
    state |> Seq.sum |> printfn "%d"

let task7 f =
    let positions = File.ReadLines("input7.txt") |> head |> String.split [","] |> map Int32.Parse |> toList
    let min = Seq.min positions
    let max = Seq.max positions
    seq { min..max }
    |> Seq.map (fun target ->
        positions |> map (fun x -> f(abs(x - target))) |> Seq.sum, target
    )
    |> Seq.min
    |> fst
    |> printfn "%d"

let task7_1() =
    task7 id

let task7_2() =
    let f n = (n + 1) * n / 2
    task7 f

let task8_1() =
    File.ReadLines("input8.txt")
    |> Seq.collect (String.split ["|"] >> Seq.last >> String.split [" "])
    |> filter (String.length >> Set([2; 3; 4; 7]).Contains)
    |> length
    |> printfn "%A"

let deduce (numbers : string seq) =
    let numbers = numbers |> toList
    let counts = numbers |> Seq.concat |> Seq.countBy id |> toList
    let e = counts |> find (snd >> (=) 4) |> fst
    let b = counts |> find (snd >> (=) 6) |> fst
    let f = counts |> find (snd >> (=) 9) |> fst
    let c = numbers |> find (length >> (=) 2) |> Seq.except [f] |> head
    let a = numbers |> find (length >> (=) 3) |> Seq.except [c; f] |> head
    let g =
        numbers
        |> find (fun k ->
            ([a; b; c; e; f] |> Seq.forall (Set k).Contains)
            && (k |> Seq.except [a; b; c; e; f] |> length |> (=) 1)
        )
        |> Seq.except [a; b; c; e; f] |> head
    let d = "abcdefg" |> Seq.except [a; b; c; e; f; g] |> head
    (a, b, c, d, e, f, g)

let task8_2() =
    File.ReadLines("input8.txt")
    |> map (
        String.split ["|"]
        >> map (String.split [" "])
        >> toList
        >> function
            | [part1; part2] ->
                let a, b, c, d, e, f, g = deduce part1
                let map =
                    [
                        [a; b; c; e; f; g]
                        [c; f]
                        [a; c; d; e; g]
                        [a; c; d; f; g]
                        [b; c; d; f]
                        [a; b; d; f; g]
                        [a; b; d; f; e; g]
                        [a; c; f]
                        [a; b; c; d; e; f; g]
                        [a; b; c; d; f; g]
                    ]
                    |> List.map Set
                let toDigit x = map |> List.findIndex ((=) (Set x))
                part2
                |> filter ((<>) "")
                |> Seq.map toDigit
                |> Seq.map string
                |> join ""
                |> Int32.Parse
            | _ -> failwith "Invalid row"
    )
    |> Seq.map (tap <| printfn "%A")
    |> Seq.sum
    |> printfn "%d"

let task9_1() =
    let map : int list list =
        File.ReadLines("input9.txt")
        |> map (Seq.map (string >> Int32.Parse) >> toList)
        |> toList

    let height = map.Length
    let width = map.[0].Length

    let tryGet (x, y) =
        if x < 0 || x >= width || y < 0 || y >= height
        then None
        else Some map.[y].[x]

    let cardinal = [
        (-1, 0)
        (1, 0)
        (0, 1)
        (0, -1)
    ]

    let lt a b = a < b
    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

    seq {
        for y in 0..height - 1 do
            for x in 0..width - 1 do
                (x, y)
    }
    |> filter (fun pos ->
        let value = pos |> tryGet |> Option.get
        cardinal |> Seq.forall (add pos >> tryGet >> Option.map (lt value) >> Option.defaultValue true)
    )
    |> Seq.map (tryGet >> Option.get >> (+) 1)
    |> Seq.sum
    |> printfn "%d"

let task9_2() =
    let map : bool list list =
        File.ReadLines("input9.txt")
        |> Seq.map (Seq.map ((=) '9') >> toList)
        |> toList

    let height = map.Length
    let width = map.[0].Length

    let get (x, y) =
        if x < 0 || x >= width || y < 0 || y >= height
        then true
        else map.[y].[x]

    let cardinal = [
        (-1, 0)
        (1, 0)
        (0, 1)
        (0, -1)
    ]

    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

    let scanned = HashSet<int * int>()
    Seq.allPairs (seq {0..width - 1}) (seq {0..height - 1})
    |> filter (get >> not)
    |> filter (scanned.Contains >> not)
    |> Seq.map (fun pos ->
        printfn "flood %A %b" pos (get pos)
        let mutable count = 0
        let mutable open_ = Set [pos]
        while open_.Count <> 0 do
            open_ <-
                open_
                |> Seq.filter (get >> not)
                |> Seq.filter (scanned.Contains >> not)
                |> Seq.collect (fun pos ->
                    count <- count + 1
                    scanned.Add pos |> ignore
                    cardinal |> Seq.map (add pos)
                )
                |> Set
        count
    )
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce ((*))
    |> printfn "%d"

    for y in 0..height - 1 do
        for x in 0..width - 1 do
            let pos = (x, y)
            if scanned.Contains pos then
                printf "-"
            elif get pos then
                printf "#"
            else printf " "
        printfn ""

let task10_1() =
    File.ReadLines "input10.txt"
    |> Seq.choose (fun line ->
        let stack = Stack<char>()
        line |> Seq.tryPick (fun ch ->
            if "[{<(".IndexOf ch >= 0
            then
                stack.Push(ch)
                None
            else
                match stack.Peek(), ch with
                | '{', '}'
                | '(', ')'
                | '[', ']'
                | '<', '>' ->
                    stack.Pop() |> ignore
                    None
                | _, ch ->
                    match ch with
                    | ')' -> 3
                    | ']' -> 57
                    | '}' -> 1197
                    | '>' -> 25137
                    | _ -> failwith "Invalid char"
                    |> Some
        )
    )
    |> Seq.sum
    |> printfn "%d"

let middle (seq : 'a seq) =
    let sorted = seq |> toList |> sort
    sorted.[sorted.Length / 2]

let task10_2() =
    File.ReadLines "input10.txt"
    |> Seq.map (fun line ->
        let stack = Stack<char>()
        let after = line |> Seq.takeWhile (fun ch ->
            if "[{<(".IndexOf ch >= 0
            then
                stack.Push(ch)
                true
            else
                match stack.Peek(), ch with
                | '{', '}'
                | '(', ')'
                | '[', ']'
                | '<', '>' ->
                    stack.Pop() |> ignore
                    true
                | _, ch -> false
        )
        let isCorrect = Seq.length after = line.Length
        if isCorrect then
            stack
            |> Seq.map (function
                | '(' -> 1
                | '[' -> 2
                | '{' -> 3
                | '<' -> 4
                | _ -> failwith "Unreachable"
            )
            |> Seq.fold (fun sum x -> sum * 5L + int64 x) 0L
        else 0
    )
    |> filter ((<>) 0L)
    |> middle
    |> printfn "%d"

module List2D =
    let size (map : 'u list list) =
        let height = map.Length
        let width = map.[0].Length
        width, height


    let containsIndex (x, y) map =
        let width, height = size map
        x < 0 || x >= width || y < 0 || y >= height

    let tryGet (x, y) map =
        if map |> containsIndex (x, y)
        then None
        else Some map.[y].[x]

    let map fn list =
        list |> List.map (List.map fn)

    let mapi (fn : (int * int) -> 't -> 'u) (list : 't list list) =
        list |> List.mapi (fun y row -> row |> List.mapi (fun x -> fn (x, y)))

module Point =
    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

    let neighbors = [
        (1, 1)
        (0, 1)
        (-1, 1)
        (1, 0)
        (-1, 0)
        (1, -1)
        (0, -1)
        (-1, -1)
    ]

let task11_1() =
    let mutable state =
        File.ReadLines "input11.txt"
        |> map (Seq.map (string >> Int32.Parse) >> toArray)
        |> toArray

    let mutable flashCount = 0
    for _ in 1..100 do
        let height = state.Length
        for y in 0..height - 1 do
            let row = state.[y]
            let width = row |> Array.length
            for x in 0..width - 1 do
                state.[y].[x] <- (state.[y].[x] + 1) % 10

        while state |> Array.exists (Array.contains 0) do
            for y in 0..height - 1 do
                let row = state.[y]
                let width = row |> Array.length
                for x in 0..width - 1 do
                    if state.[y].[x] = 0 then
                        for neighbor in Point.neighbors do
                            let x, y = neighbor |> Point.add (x, y)
                            if x >= 0 && y >= 0 && x < width && y < height then
                                let value = state.[y].[x]
                                if value > 0 then
                                    state.[y].[x] <- (value + 1) % 10
                        state.[y].[x] <- -1

        state <- state |> Array.map (Array.map (function -1 -> 0 | x -> x))
        flashCount <- flashCount + (state |> Seq.sumBy (filter ((=) 0) >> length))
    for row in state do
        for cell in row do
            printf "%d" cell
        printfn ""
    printfn "%d" flashCount
    ()

let task11_2() =
    let mutable state =
        File.ReadLines "input11.txt"
        |> map (Seq.map (string >> Int32.Parse) >> toArray)
        |> toArray

    Seq.initInfinite id
    |> Seq.map(tap(fun _ ->
        let height = state.Length
        for y in 0..height - 1 do
            let row = state.[y]
            let width = row |> Array.length
            for x in 0..width - 1 do
                state.[y].[x] <- (state.[y].[x] + 1) % 10

        while state |> Array.exists (Array.contains 0) do
            for y in 0..height - 1 do
                let row = state.[y]
                let width = row |> Array.length
                for x in 0..width - 1 do
                    if state.[y].[x] = 0 then
                        for neighbor in Point.neighbors do
                            let x, y = neighbor |> Point.add (x, y)
                            if x >= 0 && y >= 0 && x < width && y < height then
                                let value = state.[y].[x]
                                if value > 0 then
                                    state.[y].[x] <- (value + 1) % 10
                        state.[y].[x] <- -1

        state <- state |> Array.map (Array.map (function -1 -> 0 | x -> x))

    ))
    |> find (fun _ ->
        let flashCount = state |> Seq.sumBy (filter ((=) 0) >> length)
        flashCount = state.Length * state.[0].Length
    )
    |> (+) 1
    |> printfn "first flash index: %d"

let task12_1() =
    let edges : Map<string, string list>=
        File.ReadLines "input12.txt"
        |> Seq.collect (String.split ["-"] >> toList >> function [a; b] -> [a, b; b, a] | _ -> failwith "bad line")
        |> Seq.groupBy fst
        |> Map
        |> Map.mapValues (map snd >> toList)
    let rec go path =
        let cur = path |> head
        if cur = "end" then
            [ path ] |> toSeq
        else
            let descendants = edges.[cur]
            descendants
            |> filter (fun desc -> Char.IsUpper(desc.[0]) || (path |> List.contains desc |> not))
            |> Seq.collect (fun desc -> go (desc :: path))
    go ["start"]
    |> filter (head >> (=) "end")
    |> map (Seq.rev >> join ", ")
    |> tap (iter (printfn "%s"))
    |> length
    |> printfn "%d"

let inline lt a b = b < a
let inline gte a b = b >= a

let task12_2() =
    let edges : Map<string, string list>=
        File.ReadLines "input12.txt"
        |> Seq.collect (String.split ["-"] >> toList >> function [a; b] -> [a, b; b, a] | _ -> failwith "bad line")
        |> Seq.groupBy fst
        |> Map
        |> Map.mapValues (map snd >> toList)
    let isTooLarge (list : string list) =
        let counts = list |> Seq.filter (fun s -> Char.IsLower(s.[0])) |> Seq.countBy id |> Seq.filter (snd >> gte 2) |> toList
        counts.Length = 2 || counts.Length = 1 && counts |> List.exists (snd >> ((=) 3))
    let rec go path =
        let cur = path |> head
        if cur = "end" then
            [ path ] |> toSeq
        else
            let descendants = edges.[cur]
            descendants
            |> filter (fun desc -> Char.IsUpper(desc.[0]) || desc <> "start" && not (isTooLarge (desc :: path)))
            |> Seq.collect (fun desc -> go (desc :: path))
    go ["start"]
    |> filter (head >> (=) "end")
    |> length
    |> printfn "%d"

let task13_1() =
    match File.ReadAllText "input13.txt" |> String.split ["\n\n"] |> toList |> map (String.split ["\n"]) with
    | [points; folds] ->
        let points =
            points
            |> map (String.split [","] >> map Int32.Parse >> toList >> function
                | [x; y] -> (x, y)
                | _ -> failwith "Invalid line"
            )
            |> toList
        let folded =
            folds |> filter ((<>) "") |> map (String.split ["="] >> toList >> function
                | ["fold along x"; x] -> (int x, 0)
                | ["fold along y"; y] -> (0, int y)
                | x -> failwith "Invalid fold instruction"
            )
            |> Seq.fold (fun state -> function
                | divX, 0 -> state |> List.map (fun (x, y) -> ((if x < divX then x else 2 * divX - x), y))
                | 0, divY -> state |> List.map (fun (x, y) -> (x, (if y < divY then y else 2 * divY - y)))
                | _ -> failwith "Unreachable"
            ) points
        let width = folded |> map fst |> Seq.max |> (+) 1
        let height = folded |> map snd |> Seq.max |> (+) 1
        let printout = Array2D.init height width (fun _ _ -> false)
        for x, y in folded do
            printout.[y, x] <- true

        let mutable count = 0
        for y in 0..height - 1 do
            for x in 0..width - 1 do
                printf (if printout.[y, x] then "#" else " ")
                if printout.[y, x]
                then count <- count + 1
            printfn ""

        printfn "Count: %d" count
    | _ -> failwith "Invalid file"

let task14_1() =
    let [state; mapping] = File.ReadAllText("input14.txt").Split("\n\n") |> toList
    let mapping =
        mapping.Split("\n")
        |> Seq.filter ((<>) "")
        |> Seq.map(String.split [" -> "] >> toList >> function [a; b] -> (a, b))
        |> Map
    let result =
        seq {1..10}
        |> Seq.fold (fun (state : string) _ ->
            state |> toSeq |> Seq.pairwise |> Seq.map (fun (a, b) ->
                let inserted = mapping.[$"{a}{b}"]
                $"{a}{inserted}"
            )
            |> join ""
            |> (fun str -> str + (state |> Seq.last |> string))
        ) state
    let counts = result |> Seq.countBy id |> map snd |> Seq.sortDescending |> toList
    printfn "%d" (head counts - (Seq.last counts))

let task14_2() =
    let [state; mapping] = File.ReadAllText("input14.txt").Split("\n\n") |> toList
    let mapping =
        mapping.Split("\n")
        |> Seq.filter ((<>) "")
        |> Seq.map(String.split [" -> "] >> toList >> function [a; b] -> (a, b))
        |> Map
    let mutable result = Dictionary<string, int64>()
    mapping |> Map.keys |> iter (fun key -> result.Add(key, 0L))
    state
    |> toSeq
    |> Seq.pairwise
    |> Seq.distinct
    |> map (fun (a, b) -> $"{a}{b}")
    |> iter (fun key -> result.[key] <- result.[key] + 1L)
    for _ in 1..10 do
        let cur = Dictionary<string, int64>()
        mapping |> Map.keys |> iter (fun key -> cur.Add(key, 0L))
        for key in result.Keys do
            let insert = mapping.[key]
            let count = result.[key]
            cur.[$"{key.[0]}{insert}"] <- cur.[$"{key.[0]}{insert}"] + count
            cur.[$"{insert}{key.[1]}"] <- cur.[$"{insert}{key.[1]}"] + count
        result <- cur
    let cur = Dictionary<char, int64>()
    for kv in result do
        cur.[kv.Key.[0]] <- cur.GetValueOrDefault(kv.Key.[0], 0) + kv.Value
    cur.[state |> Seq.last] <- cur.GetValueOrDefault(state |> Seq.last, 0) + 1L
    let counts = cur.Values |> Seq.sortDescending |> toList
    printfn "%d" (head counts - (Seq.last counts))
*)

let applyAge (state : (string * int) list) =
    state |> List.map (fun (op, age) -> (op, age + 1))

let fifo (n : int) (ops : string list) =
    let frame = List.init n (fun _ -> (" ", 0))
    let mutable faults = 0
    let history =
        ops
        |> Seq.mapi (fun i op -> (i, op))
        |> Seq.scan (fun state (i, op) ->
                if state |> Seq.exists (fun (otherOp, age) -> otherOp = op) then
                    applyAge state
                else
                    faults <- faults + 1
                    let insertAt : int = state |> Seq.mapi(fun i (op, age) -> (i, age)) |> Seq.maxBy snd |> fst
                    state |> applyAge |> List.setAt insertAt (op, 0)
            )
            frame
        |> Seq.toList
    history, faults

let lru (n : int) (ops : string list) =
    let frame = List.init n (fun _ -> (" ", 0))
    let mutable faults = 0
    let history =
        ops
        |> Seq.mapi (fun i op -> (i, op))
        |> Seq.scan (fun state (i, op) ->
                if state |> Seq.exists (fun (otherOp, age) -> otherOp = op) |> not then
                    faults <- faults + 1
                let insertAt : int = state |> Seq.mapi(fun i (op, age) -> (i, age)) |> Seq.maxBy snd |> fst
                state |> applyAge |> List.setAt insertAt (op, 0)
            )
            frame
        |> Seq.toList
    history, faults

let secondChance (n : int) (ops : string list) =
    let frame = List.init n (fun _ -> (" ", 0))
    let mutable faults = 0
    let history =
        ops
        |> Seq.mapi (fun i op -> (i, op))
        |> Seq.scan (fun state (i, op) ->
                if state |> Seq.exists (fun (otherOp, age) -> otherOp = op) then
                    applyAge state
                else
                    faults <- faults + 1
                    let insertAt : int = state |> Seq.mapi(fun i (op, age) -> (i, age)) |> Seq.maxBy snd |> fst
                    state |> applyAge |> List.setAt insertAt (op, 0)
            )
            frame
        |> Seq.toList
    history, faults

let optimal (n : int) (ops : string list) =
    let frame = List.init n (fun _ -> " ")
    let mutable faults = 0
    let history =
        ops
        |> Seq.mapi (fun i op -> (i, op))
        |> Seq.scan (fun state (index, op) ->
                if state |> Seq.exists (fun otherOp -> otherOp = op) |> not then
                    faults <- faults + 1
                    // 1. find first not in list
                    // 2. find second
                    let insertAt =
                        state |> Seq.mapi(fun i op ->
                            let age = ops |> Seq.skip index |> Seq.tryFindIndex (fun x -> x = op) |> Option.defaultValue Int32.MaxValue
                            i, age
                        ) |> Seq.maxBy snd |> fst
                    state |> List.setAt insertAt op
                else state
            )
            frame
        |> Seq.toList
    history, faults


let show (history : (string * int) list list) =
    for frameIndex in 0..history.[0].Length - 1 do
        for timeIndex in 0..history.Length - 1 do
            printf "%s | " (fst history.[timeIndex].[frameIndex])
        printfn ""

let show2 (history : string list list) =
    for frameIndex in 0..history.[0].Length - 1 do
        for timeIndex in 0..history.Length - 1 do
            printf "%s | " history.[timeIndex].[frameIndex]
        printfn ""
        
let oldTask() = 
    let input = "(6)(5)(6)(2)(4)(6)(7)(1)(4)(5)(4)(6)(8)(6)(1)(6)(1)(5)(6)(5)"
    let input = input.Replace("(", "").Split(")") |> filter (fun x -> x <> "") |> toList
    let history, faults = optimal 4 input
    show2 history
    printfn "Faults: %d" faults

[<EntryPoint>]
let main argv =
    oldTask()
    0
