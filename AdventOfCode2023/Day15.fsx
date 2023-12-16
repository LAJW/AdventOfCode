#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System
open System.Collections.Generic

open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let enumerate seq = seq |> Seq.mapi (fun i v -> i, v)

let data = """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7""" |> String.split ["\n"] |> toList

let data = File.ReadAllText "./AdventOfCode2023/data15.txt"

let hash (str : string) =
    (0, str) ||> Seq.fold (fun state ch -> ((state + int ch) * 17) % 256)

data.Split "," |> Seq.map hash |> sum |> printfn "Part 1: %d"

let tryParseInt (s : string) =
    match Int32.TryParse s with
    | false, _ -> None
    | true, value -> Some value

data.Split ","
|> Seq.map (String.split ["="; "-"] >> toList >> function [ id; lens ] -> (id, tryParseInt lens))
|> Seq.fold (fun (state : Map<int, (string * int) list>) (id, lens) ->
    let h = hash id
    match lens, (state |> Map.tryFind h) with
    | None, Some box -> state.Add(h, box |> List.filter (fst >> (<>) id))
    | None, None -> state
    | Some lens, Some box ->
        if box |> Seq.exists (fst >> (=) id) then
            state.Add(h, box |> List.map (fun (curId, curLens) -> if curId = id then (id, lens) else (curId, curLens)))
        else state.Add(h, (id, lens) :: box)
    | Some lens, None ->
        state.Add(h, [id, lens])
        
) Map.empty
|> Map.toSeq
|> Seq.bind (fun (h, box) -> box |> Seq.rev |> Seq.mapi (fun index (_, lens) -> (h + 1) * (index + 1) * lens))
|> Seq.sum
|> printfn "Part 2: %d"
