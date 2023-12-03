module Day2

open System
open FSharpPlus
open System.IO

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

// let data = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
// Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
// Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
// Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
// Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".Split("\n")

let data = File.ReadAllLines "./data2.txt"
let maxR, maxG, maxB = (12, 13, 14)

let getId (game: string) = game.Split(": ").[0].Split(" ").[1] |> Int32.Parse

let parse =
    String.split [": "; "; "; ", "]
    >> Seq.tail
    >> map (String.split [" "] >> toList >> function
        | [amount; "red"] -> (Int32.Parse amount, 0, 0)
        | [amount; "green"] -> (0, Int32.Parse amount, 0)
        | [amount; "blue"] -> (0, 0, Int32.Parse amount)
        | other -> failwith ("Unknown values: " + (other |> join "; ")))

data
|> filter (parse >> Seq.forall (fun (r, g, b) -> r <= maxR && g <= maxG && b <= maxB))
|> map getId
|> sum
|> printfn "Part 1: %d"

data
|> map (fun row ->
    let results = parse row
    let r = results |> map (fun (r, _, _) -> r) |> Seq.max
    let g = results |> map (fun (_, g, _) -> g) |> Seq.max
    let b = results |> map (fun (_, _, b) -> b) |> Seq.max
    r * g * b
)
|> sum
|> printfn "Part 2: %d"
