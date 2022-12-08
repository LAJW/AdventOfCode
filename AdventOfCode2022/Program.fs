module FSharpPlayground
open FSharpPlus
open System.IO
open System
open System.Collections.Generic

let day1_1() =
    File.ReadLines("./data.txt")
    |> Seq.chunkBy(fun x -> x = "")
    |> Seq.toList
    |> Seq.filter (fun (empty, _) -> not empty)
    |> Seq.map snd
    |> Seq.map (Seq.map int >> Seq.sum)
    |> Seq.max
    |> printfn "%d"
    
let day1_2() =
    File.ReadLines("./data.txt")
    |> Seq.chunkBy(fun x -> x = "")
    |> Seq.toList
    |> Seq.filter (fun (empty, _) -> not empty)
    |> Seq.map snd
    |> Seq.map (Seq.map int >> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> printfn "%d"

type Shape =
    | Rock
    | Paper
    | Scissors

let day2_1() =
    let parse = function
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> failwith "bad input"
    let score = function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    let round = function
        | Rock, Paper -> 0
        | Rock, Scissors -> 6
        | Paper, Rock -> 6
        | Paper, Scissors -> 0
        | Scissors, Rock -> 0
        | Scissors, Paper -> 6
        | _ -> 3
    File.ReadLines("data/day2.txt")
    |> Seq.map(String.split [" "] >> Seq.map(parse) >> Seq.toList)
    |> Seq.map(function [theirs; mine] -> score mine + round (mine, theirs) | _ -> failwith "bad input")
    |> Seq.sum
    |> printfn "%d"

type Outcome =
    | Win
    | Lose
    | Draw

let day2_2() =
    let parseRight = function
        | "X" -> Lose
        | "Y" -> Draw
        | "Z" -> Win
        | _ -> failwith "bad input"
    let parseLeft = function
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> failwith "bad input"
    let score = function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    let round = function
        | Win -> 6
        | Lose -> 0
        | Draw -> 3
    let pick = function
        | Rock, Lose -> Scissors
        | Rock, Win -> Paper
        | Paper, Lose -> Rock
        | Paper, Win -> Scissors
        | Scissors, Lose -> Paper
        | Scissors, Win -> Rock
        | shape, Draw -> shape
    File.ReadLines("data/day2.txt")
    |> Seq.map(fun pair ->
        match pair.Split(' ') with
        | [| left; right |] -> (parseLeft left, parseRight right)
        | _ -> failwith "bad input"
    )
    |> Seq.map(fun decision -> score (pick decision) + round (snd decision))
    |> Seq.sum
    |> printfn "%d"

module Day3 =
    let priority (ch: char) =
        if Char.IsLower ch then int ch - int 'a' + 1
        else if Char.IsUpper ch then int ch - int 'A' + 27
        else failwith $"Invalid char: {ch}"
    let part1() =
        File.ReadLines("data/day3.txt")
        |> Seq.map(fun chars ->
            let middle = chars.Length / 2
            let firstHalf = chars.Substring(0, middle) |> Set
            let secondHalf = chars.Substring(middle) |> Set
            match firstHalf |> Set.intersect secondHalf |> Set.toList with
            | [head] -> head |> priority
            | _ -> failwith "Multiple common elements"
        )
        |> Seq.sum
        |> printfn "Result: %d"
    let part2() =
        File.ReadLines("data/day3.txt")
        |> Seq.chunkBySize 3
        |> Seq.map(fun group ->
            match group |> Seq.map Set |> Set.intersectMany |> Seq.toList with
            | [head] -> head |> priority
            | _ -> failwith "Multiple common elements"
        )
        |> Seq.sum
        |> printfn "Result: %d"

module Day4 =
    let part1() =
        File.ReadLines("data/day4.txt")
        |> Seq.map (String.split [","] >> Seq.map(String.split ["-"] >> Seq.map int >> toList) >> toList)
        |> Seq.filter (function [[a; b]; [c; d]] -> a <= c && b >= d || a >= c && b <= d | _ -> failwith "bad input")
        |> Seq.length
        |> printfn "Result: %d"

    let part2() =
        File.ReadLines("data/day4.txt")
        |> Seq.map (String.split [","] >> Seq.map(String.split ["-"] >> Seq.map int >> toList) >> toList)
        |> Seq.filter (function [[a; b]; [c; d]] -> b >= c && a <= d | _ -> failwith "bad input")
        |> Seq.length
        |> printfn "Result: %d"

module Day5 =
    let part1() =
        let lines = File.ReadAllLines("data/day5.txt")
        let emptyLine = lines |> findIndex ((=) "")
        let stacks = lines[0..emptyLine - 1]
        let instructions = lines[emptyLine + 1..lines.Length - 1]
        let stackCount = stacks |> Seq.last |> String.split [" "] |> filter ((<>) "") |> Seq.last |> int
        
        let state = List.init stackCount (fun _ -> Stack<char>())
        
        stacks |> Seq.rev |> Seq.tail |> Seq.iter(fun line ->
            line
            |> Seq.chunkBySize 4
            |> Seq.map(Seq.toList >> function _ :: ch :: _ -> ch | _ -> failwith "bad input")
            |> Seq.mapi(fun index value -> index, value)
            |> Seq.filter(snd >> (<>) ' ')
            |> Seq.iter(fun (index, value) -> state[index].Push(value))
        )
        
        for instruction in instructions do
            match instruction.Split(' ') |> toList with
            | _ :: count :: _ :: source :: _:: dest :: _ ->
                let count = int count
                let source = int source - 1
                let dest = int dest - 1
                for _ in 1..count do
                    let value = state[source].Pop()
                    state[dest].Push(value)
            | x -> failwith $"Invalid line: {x}"
        
        state |> map Seq.head |> Seq.toArray |> String |> printfn "%s"
        
    let part2() =
        let lines = File.ReadAllLines("data/day5.txt")
        let emptyLine = lines |> findIndex ((=) "")
        let stacks = lines[0..emptyLine - 1]
        let instructions = lines[emptyLine + 1..lines.Length - 1]
        let stackCount = stacks |> Seq.last |> String.split [" "] |> filter ((<>) "") |> Seq.last |> int
        
        let state = List.init stackCount (fun _ -> Stack<char>())
        
        stacks |> Seq.rev |> Seq.tail |> Seq.iter(fun line ->
            line
            |> Seq.chunkBySize 4
            |> Seq.map(Seq.toList >> function _ :: ch :: _ -> ch | _ -> failwith "bad input")
            |> Seq.mapi(fun index value -> index, value)
            |> Seq.filter(snd >> (<>) ' ')
            |> Seq.iter(fun (index, value) -> state[index].Push(value))
        )
        
        for instruction in instructions do
            match instruction.Split(' ') |> toList with
            | _ :: count :: _ :: source :: _:: dest :: _ ->
                let count = int count
                let source = int source - 1
                let dest = int dest - 1
                
                let chunk = seq { for _ in 1..count do yield state[source].Pop() } |> Seq.rev
                for value in chunk do state[dest].Push(value)
            | x -> failwith $"Invalid line: {x}"
        
        state |> map Seq.head |> Seq.toArray |> String |> printfn "%s"

module Day6 =
    let findFirstUniqueSequenceOfLength (length: int) (line: string) =
        Seq.init line.Length (fun index -> line[index .. index + length - 1])
        |> findIndex(distinct >> Seq.length >> (=) length)
        |> (+) length
        
    let part1() =
        File.ReadLines("data/day6.txt")
        |> map (findFirstUniqueSequenceOfLength 4)
        |> iter (printfn "Result %d")

    let part2() =
        File.ReadLines("data/day6.txt")
        |> map (findFirstUniqueSequenceOfLength 14)
        |> iter (printfn "Result %d")

module Day7 =
    type Node =
        | File of string * int
        | Directory of string * List<Node>
        
    let parseTerminal() =
        let root = Directory("", List<Node>())
        let mutable path = Stack<Node>([root])
        for line in File.ReadLines("data/day7.txt") do
            match line.Split(' ') |> Seq.toList with
            | ["$"; "cd"; "/"] ->
                path <- Stack<Node>([root])
            | ["$"; "cd"; ".."] ->
                match path.TryPop() with
                | true, _ -> ()
                | false, _ -> failwith "Can't go up the root"
            | ["$"; "cd"; dirName] ->
                match path.Peek() with
                | Directory(_name, children) ->
                    children |> Seq.tryFind(function
                        | Directory(name, _children) when name = dirName -> true
                        | _ -> false
                    )
                    |> function
                        | Some child -> path.Push(child)
                        | None -> failwith ("Could not find directory" + dirName)
                | File _ -> failwith "Unreachable"
            | ["$"; "ls"] ->
                ()
            | ["dir"; dirName] ->
                match path.Peek() with
                | Directory(_name, children) ->
                    children.Add(Directory(dirName, List<Node>()))
                | File _ -> failwith "Unreachable"
            | [size; fileName] ->
                match path.Peek() with
                | Directory(_name, children) ->
                    children.Add(File(fileName, int size))
                | File _ -> failwith "Unreachable"
            | _ -> failwith ("Unknown command " + line)
        root

    let rec getTotalSize node =
        match node with
        | File(_name, size) -> size
        | Directory(_name, children) -> children |> map getTotalSize |> sum

    let rec getSizes node =
        seq {
            match node with
            | File _ -> ()
            | Directory(_name, children) ->
                yield getTotalSize node
                for child in children do yield! getSizes child
        }
        
    let part1() =
        let root = parseTerminal()
        getSizes root
        |> filter ((>=) 100_000)
        |> Seq.sum
        |> printfn "Result: %d"

    let part2() =
        let root = parseTerminal()
        let totalSize = getTotalSize root
        let sizeAvailable = 70000000 - totalSize
        let sizeToDelete = 30000000 - sizeAvailable

        getSizes root
        |> filter ((<=) sizeToDelete)
        |> Seq.min
        |> printfn "Result: %d"

[<EntryPoint>]
let main _argv =
    Day7.part2()
    0
