module AdventOfCode2024.day15

// #r "nuget: FSharpPlus"
// #r "./bin/Debug/net8.0/AdventOfCode2024.dll"
open System.IO
open System
open AdventOfCode2024.Utils
open FSharpPlus
open System.Threading

let arrowToDirection ch =
    match ch with
    | '^' -> Vec(0, -1)
    | '>' -> Vec(1, 0)
    | '<' -> Vec(-1, 0)
    | 'v' -> Vec(0, 1)
    | _ -> failwith "bad starting position"
    
let join (delim: string) (strs: string seq) = String.Join(delim, strs)

let show (grid: Grid<char>) =
    Console.Clear()
    Console.WriteLine(grid.Data |> Seq.map String.ofArray |> join "\n")

let run1 () =
    // let text = File.ReadAllText("./AdventOfCode2024/day15.txt").TrimEnd()
    let text = File.ReadAllText("day15.txt").TrimEnd()

    let [| mapStr; instructions |] = text.Split("\n\n")
    let grid = mapStr.Split("\n") |> Grid.fromLines
    let moves = instructions.Split("\n") |> join "" |> Seq.map arrowToDirection
    let mutable pos = grid |> Grid.enumerate |> Seq.find (snd >> (=) '@') |> fst

    let rec push direction pos =
        let next = pos + direction

        match grid[next] with
        | '#' -> false
        | 'O' ->
            let pushed = push direction next

            if pushed then
                grid[next] <- grid[pos]
                grid[pos] <- '.'
                true
            else
                false
        | '.' ->
            grid[next] <- grid[pos]
            grid[pos] <- '.'
            true
        | _ -> failwith "unreachable"

    for move in moves do
        if push move pos then
            pos <- pos + move

    grid
    |> Grid.enumerate
    |> Seq.filter (snd >> is 'O')
    |> Seq.map (fun (pos, _) -> pos.Y * 100 + pos.X)
    |> Seq.sum
    |> printfn "%d"

let run2 () =
    // let text = File.ReadAllText("./AdventOfCode2024/day15.txt").TrimEnd()
    let text = File.ReadAllText("day15.txt").TrimEnd()
    
    let text = text.Replace("#", "##").Replace(".", "..").Replace("O", "[]").Replace("@", "@.")

    let [| mapStr; instructions |] = text.Split("\n\n")
    let grid = mapStr.Split("\n") |> Grid.fromLines
    let moves = instructions.Split("\n") |> join "" |> Seq.map arrowToDirection
    let mutable pos = grid |> Grid.enumerate |> Seq.find (snd >> (=) '@') |> fst
    
    let rec pushVertical direction (positions: Vec Set) =
        let nexts = positions |> Seq.map((+) direction)
        if positions.IsEmpty then true
        else if nexts |> Seq.exists (fun next -> grid[next] = '#') then false
        else
            let nextLayer =
                nexts |> Seq.collect (fun next ->
                    match grid[next] with
                    | '[' -> [next; next + Vec.right]
                    | ']' -> [next; next + Vec.left]
                    | '.' -> List.empty
                    | _ -> failwith "unreachable"
                )
                |> Set

            if pushVertical direction nextLayer then
                for pos in positions do
                    grid[pos + direction] <- grid[pos]
                    grid[pos] <- '.'
                true
            else
                false

    // Same as previous push
    let rec pushHorizontal (direction : Vec) (pos: Vec) =
        let next = pos + direction
        match grid[next] with
        | '#' -> false
        | '[' | ']' ->
            let pushed = pushHorizontal direction next

            if pushed then
                grid[next] <- grid[pos]
                grid[pos] <- '.'
                true
            else
                false
        | '.' ->
            grid[next] <- grid[pos]
            grid[pos] <- '.'
            true
        | _ -> failwith "unreachable"

    let push (direction : Vec) (pos : Vec) =
        if direction.Y = 0 then
            pushHorizontal direction pos
        else
            pushVertical direction (Set[pos])

    for move in moves do
        if push move pos then
            pos <- pos + move

    grid
    |> Grid.enumerate
    |> Seq.filter (snd >> is '[')
    |> Seq.map (fun (pos, _) -> pos.Y * 100 + pos.X)
    |> Seq.sum
    |> printfn "%d"
