module AdventOfCode2024.day6

// #r "nuget: FSharpPlus"
open System
open System.Collections.Generic
open System.IO
open FSharpPlus

let turnRight(dir: int * int) =
    match dir with
    | 1, 0 -> 0, 1
    | 0, 1 -> -1, 0
    | -1, 0 -> 0, -1
    | 0, -1 -> 1, 0
    | _ -> failwith "function only works with directional vectors"
    
let add(a: int * int) (b: int * int) =
    let xa, ya = a
    let xb, yb = b
    (xa + xb, ya + yb)

let tryGet (pos : int * int) (grid : string array) =
    let x, y = pos
    if x >= 0 && y >= 0 && x < grid[0].Length && y < grid.Length then
        Some(grid[y][x])
    else None
    
let arrowToDirection ch =
    match ch with
    | '^' -> 0, -1
    | '>' -> 0, 1
    | '<' -> -1, 0
    | 'v' -> 0, 1
    | _ -> failwith "bad starting position"

let getStartingPosition (grid : string array) =
    seq {0..grid.Length - 1} |> Seq.pick (fun y ->
        seq {0..grid[y].Length - 1} |> Seq.tryPick (fun x ->
            let pos = x, y
            match grid |> tryGet pos |> Option.get with
            | '^' | '>' | '<' | 'v' -> Some(pos)
            | _ -> None
        )
    )

let walk (startingPosition : int * int) (startingDirection : int * int) (grid : string array)= 
    let mutable direction = startingDirection
    let mutable pos = startingPosition
    let visited = HashSet<(int * int) * (int * int)>()
    while grid |> tryGet pos |> Option.isSome && not(visited.Contains(pos, direction)) do
        let next = pos |> add direction
        match grid |> tryGet next with
        | Some('#') ->
            direction <- direction |> turnRight
        | _ ->
            visited.Add(pos, direction) |> ignore
            pos <- pos |> add direction
    visited, pos

let run1 () =
    let grid = File.ReadAllLines("data6.txt")
    let startingPosition = getStartingPosition grid
    let startingDirection = grid |> tryGet startingPosition |> Option.get |> arrowToDirection
    let positions = grid |> walk startingPosition startingDirection |> fst |> Seq.map fst |> Set
    printfn "%d" positions.Count
   
let run2 () =
    let grid = File.ReadAllLines("data6.txt")
    let startingPosition = getStartingPosition grid
    let startingDirection = grid |> tryGet startingPosition |> Option.get |> arrowToDirection
    let positions = grid |> walk startingPosition startingDirection |> fst |> Seq.map fst |> Set

    let loops(grid : string array) =
        let finalPos = grid |> walk startingPosition startingDirection |> snd
        grid |> tryGet finalPos |> Option.isSome

    let loopingPositions = seq {
        for x, y in positions do
            let newGrid =
                grid |> Array.mapi (fun i row ->
                    if i = y then
                        row |> Seq.mapi (fun j cell -> if j = x then '#' else cell) |> String.ofSeq
                    else row
                )
            if loops newGrid then
                yield x, y
    }
        
    let count = Seq.length loopingPositions
    printfn "%d" count
