module AdventOfCode2024.day15

open System.IO
open System
open AdventOfCode2024.Utils
open FSharpPlus

let show (grid: Grid<char>) =
    Console.Clear()
    Console.WriteLine(grid.Data |> Seq.map String.ofArray |> String.join "\n")

let preprocess (mapText : string) = 
    mapText.Replace("#", "##").Replace(".", "..").Replace("O", "[]").Replace("@", "@.")

// returns true if elements were pushed
let push (direction : Vec) (pos : Vec) (grid : Grid<char>): bool =
    let rec iter (positions: Vec Set) =
        let nexts = positions |> Seq.map((+) direction)
        if positions.IsEmpty then true
        else if nexts |> Seq.exists (fun next -> grid[next] = '#') then false
        else
            let nextLayer =
                nexts |> Seq.collect (fun next ->
                    match grid[next], direction.asPair with
                    | '[', (0, _) -> [next; next + Vec.right]
                    | ']', (0, _) -> [next; next + Vec.left]
                    | ('O' | '[' | ']'), _ -> [next]
                    | '.', _ -> List.empty
                    | _ -> failwith "unreachable"
                )
                |> Set

            let wasPushed = iter nextLayer
            if wasPushed then
                for pos in positions do
                    grid[pos + direction] <- grid[pos]
                    grid[pos] <- '.'
            wasPushed
    iter (Set [pos])

let solve preprocess =
    let text = File.ReadAllText("day15.txt").TrimEnd()

    let [| mapStr; instructions |] = text.Split("\n\n")
    let moves = instructions.Split("\n") |> String.join "" |> Seq.map Vec.fromArrow
    
    let grid = mapStr |> preprocess |> String.split ["\n"] |> Seq.toArray |> Grid.fromLines
    let mutable pos = grid |> Grid.enumerate |> Seq.find (snd >> (=) '@') |> fst

    for move in moves do
        if grid |> push move pos then
            pos <- pos + move

    show grid

    grid
    |> Grid.enumerate
    |> Seq.choose (function
        | pos, ('[' | 'O') -> Some(pos.Y * 100 + pos.X)
        | _ -> None)
    |> Seq.sum
    |> printfn "%d"
    

let run1 () = solve id
let run2 () = solve preprocess
