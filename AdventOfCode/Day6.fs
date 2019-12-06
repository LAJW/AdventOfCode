module Day6
open Extensions

let parse : string seq -> Map<string, string> =
    Seq.map (String.split ')' >> Seq.toList >> function [parent; child] -> child, parent) >> Map

let rec depth (child : 'u) (map : Map<'u, 'u>) =
    match map.TryFind child with
    | Some parent -> map |> depth parent |> ((+) 1)
    | None -> 0
    
let allDepths (map : Map<'u, 'u>) =
    map |> Map.toSeq |> Seq.map (fun (child, parent) -> map |> depth child)

let rec path (child : 'u) (map : Map<'u, 'u>) =
    match map.TryFind child with
    | Some parent -> (map |> path parent) @ [parent]
    | None -> []

let longestCommonSubsequence (a : 'u seq) (b : 'u seq) =
    Seq.zip a b |> Seq.takeWhile (fun (a, b) -> a = b)
    
let countOrbits map = map |> allDepths |> Seq.sum

let distance (a : string) (b : string) (map : Map<string, string>) =
    let pathA = map |> path a
    let pathB = map |> path b
    let commonLength = longestCommonSubsequence pathA pathB |> Seq.length
    pathA.Length + pathB.Length - (2 * commonLength)

