#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System
open System.Collections.Generic

open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let enumerate seq = seq |> Seq.mapi (fun i v -> i, v)

let data = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1""" |> String.split ["\n"] |> toList

let data = File.ReadAllLines "./AdventOfCode2023/data12.txt" |> toList

let replaceSlots (placeholder : 'T) (values : 'T list) (lst : 'T list) =
    let rec iter result values lst =
        match lst with
        | first :: rest when first = placeholder ->
            iter (head values :: result) (List.tail values) rest
        | first :: rest ->
            iter (first :: result) values rest
        | [] -> List.rev result
    iter [] values lst

let replaceSlotsStr (ch : char) (values : string) (str : string) =
    replaceSlots ch (toList values) (toList str) |> toArray |> String

let getGroups (row : string) =
    row.Split(".") |> Seq.map String.length |> Seq.filter ((<>) 0) |> toList

let processRow (row : string) =
    let [| springs; groupsText |] = row.Split(" ")

    let groups = groupsText.Split(",") |> toList |> map Int32.Parse

    let count = springs |> Seq.filter ((=) '?') |> length

    seq {0..(pown 2 count) - 1} |> map (fun value ->
        (sprintf "%B" value).Replace('1', '.').Replace('0', '#').PadLeft(count).Replace(' ', '#')
    )
    |> Seq.map (fun permutation -> springs |> replaceSlotsStr '?' permutation)
    |> map getGroups
    |> filter ((=) groups)
    |> Seq.length

data |> map processRow |> Seq.sum |> printfn "Part 1: %d"

// part 2

let memory = Dictionary<char list * int list * int, int64>()

let rec f (chars : char list) (groups : int list) (lengthSoFar : int) : int64 =
    match memory.TryGetValue((chars, groups, lengthSoFar)) with
    | true, value -> value
    | false, _ ->
        let result =
            match chars with
            | '.' :: rest ->
                if lengthSoFar <= 0 then
                    f rest groups 0
                else 0L
            | '#' :: rest ->
                if lengthSoFar = -1 then
                    0L
                elif groups.IsEmpty then
                    0L
                elif lengthSoFar + 1 = head groups then
                    f rest (List.tail groups) -1
                elif lengthSoFar + 1 < head groups then
                    f rest groups (lengthSoFar + 1)
                else 0L
            | '?' :: rest when lengthSoFar = -1 ->
                (f rest groups 0)
            | '?' :: rest when lengthSoFar = 0 ->
                (f rest groups 0) + (f ('#' :: rest) groups lengthSoFar)
            | '?' :: rest when lengthSoFar > 0 ->
                (f ('#' :: rest) groups lengthSoFar)
            | [] when groups.IsEmpty -> 1L
            | _ -> 0L
        memory[(chars, groups, lengthSoFar)] <- result
        result

data |> map (fun row ->
    let [| springs; groupsText |] = row.Split(" ")
    f (toList springs) (groupsText.Split(",") |> map Int32.Parse |> toList) 0
)
|> Seq.sum
|> printfn "Part 1: %d"

data |> map (fun row ->
    let [| springs; groupsText |] = row.Split(" ")
    let springs = Seq.init 5 (fun _ -> springs) |> join "?"
    let groupsText = Seq.init 5 (fun _ -> groupsText) |> join ","
    f (toList springs) (groupsText.Split(",") |> map Int32.Parse |> toList) 0
)
|> Seq.sum
|> printfn "Part 2: %d"
