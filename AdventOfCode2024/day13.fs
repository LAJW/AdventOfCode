module AdventOfCode2024.day13

open System.IO
open System.Text.RegularExpressions
open FSharpPlus
open AdventOfCode2024.Utils

let parseButton str =
    match Regex.Match(str, @"Button [AB]: X\+(\d+), Y\+(\d+)").Groups |> Seq.toList with
    | [ _; x; y ] -> (int x.Value, int y.Value)
    | s -> failwith ("unreachable" + s.ToString())

let parsePrize str =
    match Regex.Match(str, @"Prize: X=(\d+), Y=(\d+)").Groups |> Seq.toList with
    | [ _; x; y ] -> (int x.Value, int y.Value)
    | s -> failwith ("unreachable" + s.ToString())

let inside (bounds: Vec) (vec: Vec) = vec.X <= bounds.X && vec.Y <= bounds.Y

let tryMin (seq: seq<'a>) : option<'a> =
    if Seq.isEmpty seq then None else seq |> Seq.min |> Some

let solve offset =
    let text = File.ReadAllText("day13.txt").TrimEnd()
    let segments = text.Split("\n\n")

    let machines =
        segments
        |> Seq.map (fun segment ->
            match segment.Split("\n") with
            | [| a; b; prize |] ->
                let buttonA = parseButton a |> Vec
                let buttonB = parseButton b |> Vec
                let prize = parsePrize prize |> Vec
                buttonA, buttonB, prize
            | _ -> failwith "Unreachable")
        |> Seq.toList

    machines
    |> Seq.choose (fun machine ->
        let u, v, t = machine
        let t1 = int64 t.X + offset
        let t2 = int64 t.Y + offset
        let x1 = int64 u.X
        let x2 = int64 v.X
        let y1 = int64 u.Y
        let y2 = int64 v.Y
        
        let W = x1 * y2 - x2 * y1
        let Wa = y2 * t1 - x2 * t2
        let Wb = x1 * t2 - y1 * t1
        let a = Wa / W
        let b = Wb / W
        
        if x1 * a + x2 * b = t1 && y1 * a + y2 * b = t2 then
            Some(a * 3L + b)
        else None
    )
    |> Seq.toList
    |> Seq.sum
    |> printfn $"%d"

let part1() = solve 0
let part2() = solve 10000000000000L
