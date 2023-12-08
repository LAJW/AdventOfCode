module Day6

open System

open System.Text.RegularExpressions
open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

// let data = """Time:      7  15   30
// Distance:  9  40  200""".Split("\n")

let data = """Time:        62     73     75     65
Distance:   644   1023   1240   1023""".Split("\n")

let races =
    let [|times; distances|] = data |> map (String.split [" "] >> Seq.tail >> filter ((<>) "") >> map Int32.Parse)
    zip times distances

let simulate time =
    seq {0..time} |> map (fun x ->
        let speed = time - x
        let distance = speed * x
        distance
    )
    
races |> Seq.map (fun (time, distance) -> simulate time |> filter (fun d -> d > distance) |> Seq.length) |> Seq.reduce (*) |> printfn "Part 1: %d"

// Part 2

let [|time; distance|] = data |> map (String.split [" "] >> Seq.tail >> join "" >> Int64.Parse)

let lower =
    let mutable lo = 0L
    let mutable hi = time / 2L
    while hi - lo <= 1 do
        let x = lo + (hi - lo) / 2L
        let result = (time - x) * x 
        if result < distance then
            lo <- x
        else if result > distance then
            hi <- x

    seq {lo..hi}
    |> Seq.find (fun x -> (time - x) * x > distance)

let higher =
    let mutable lo = time / 2L
    let mutable hi = time
    while hi - lo <= 1 do
        let x = lo + (hi - lo) / 2L
        let result = (time - x) * x 
        if result > distance then
            lo <- x
        else if result < distance then
            hi <- x

    seq {lo..hi}
    |> Seq.find (fun x -> (time - x) * x < distance)

printfn $"Part 2: {lower} {higher} {higher - lower}"
