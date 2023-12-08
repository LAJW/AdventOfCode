// #r "./bin/Debug/net7.0/FSharpPlus.dll"
module Day5

open System

open System.Text.RegularExpressions
open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

// let data = """seeds: 79 14 55 13
//
// seed-to-soil map:
// 50 98 2
// 52 50 48
//
// soil-to-fertilizer map:
// 0 15 37
// 37 52 2
// 39 0 15
//
// fertilizer-to-water map:
// 49 53 8
// 0 11 42
// 42 0 7
// 57 7 4
//
// water-to-light map:
// 88 18 7
// 18 25 70
//
// light-to-temperature map:
// 45 77 23
// 81 45 19
// 68 64 13
//
// temperature-to-humidity map:
// 0 69 1
// 1 0 69
//
// humidity-to-location map:
// 60 56 37
// 56 93 4"""

let data = File.ReadAllText "./data5.txt"

let sections = data.Split("\r\n\r\n")

let seeds = sections.[0].Split(": ").[1].Split(" ") |> map Int64.Parse

let transitions = sections[1..]

let transition = transitions[0]

let solve state =
    (state, transitions) ||> Seq.fold(fun state transition ->
        let parsed =
            transition.Split("\r\n")
            |> Seq.filter ((<>) "")
            |> Seq.tail
            |> map (String.split [" "] >> toList >> map Int64.Parse >> function [dest; source; length] -> dest, source, length)
            |> toList
        state |> List.map (fun loc ->
            parsed
            |> tryFind (fun (dest, start, length) -> loc >= start && loc < start + length)
            |> function
                | Some (dest, start, _) -> loc - start + dest
                | None -> loc
        )
    )
    |> Seq.min

seeds |> toList |> solve |> printfn "Part 1: %d"
seeds |> Seq.chunkBySize 2 |> Seq.bind (toList >> function [ start; dest ] -> seq {start..start + dest - 1L}) |> toList |> solve |> printfn "Part 2: %d"
