module AdventOfCode2024.day9

// #r "nuget: FSharpPlus"
open System.IO
open System
open FSharpPlus
open System.Collections.Generic

let render (files: int array) =
    for file in files do
        let ch = if file >= 0 then '0' + (char file) else '.'
        printf "%c" ch

let run1 () =
    // let lines = File.ReadAllLines("./AdventOfCode2024/data9.txt")
    let lines = File.ReadAllLines("data9.txt")

    let files =
        lines[0]
        |> Seq.map (fun ch -> ch - '0' |> int)
        |> Seq.chunkBySize 2
        |> Seq.mapi (fun index sizeAndGap ->
            let size, gap =
                match sizeAndGap with
                | [| size |] -> size, 0
                | [| size; gap |] -> size, gap
                | _ -> failwith "Unreachable"

            Seq.append (Seq.init size (fun _ -> index)) (Seq.init gap (fun _ -> -1)))
        |> Seq.concat
        |> Seq.toArray

    let mutable i = files.Length - 1
    let mutable j = 0

    while i > j do
        while j < i && files[j] >= 0 do
            j <- j + 1

        if files[j] < 0 then
            files[j] <- files[i]
            files[i] <- -1

        i <- i - 1

    files
    |> Seq.takeWhile (fun file -> file >= 0)
    |> Seq.mapi (fun i value -> (int64 i) * (int64 value))
    |> Seq.sum
    |> printfn "%d"

// Represents our file
type F = { mutable Id: int; mutable Size: int; mutable Gap: int; mutable moved : bool }

let render2 (files: F List) =
    for file in files do
        for _ in 0..file.Size - 1 do
            printf "%d" file.Id
            
        for _ in 0..file.Gap - 1 do
            printf "."
    printfn ""

let checksum (files: F List) =
    let mutable i = 0L
    let mutable sum = 0L
    for file in files do
        for _ in 0..file.Size - 1 do
            sum <- sum + i * (int64 file.Id)
            i <- i + 1L
        i <- i + int64 file.Gap
    sum
    

let run2 () =
    // let lines = File.ReadAllLines("./AdventOfCode2024/data9.txt")
    let lines = File.ReadAllLines("data9.txt")

    let files =
        lines[0]
        |> Seq.map (fun ch -> ch - '0' |> int)
        |> Seq.chunkBySize 2
        |> Seq.mapi (fun index sizeAndGap ->
            match sizeAndGap with
            | [| size |] -> { Id = index; Size = size; Gap = 0; moved = false }
            | [| size; gap |] -> { Id = index; Size = size; Gap = gap; moved = false }
            | _ -> failwith "Unreachable")
        |> List

    let mutable i = files.Count - 1
    
    while i > 0 do
        let cur = files[i]
        if cur.moved then
            i <- i - 1
        else
            match files |> Seq.tryFindIndex(fun file -> file.Gap >= cur.Size) with
            | Some indexBefore when indexBefore < i ->
                // Replace a file with a gap
                files[i - 1].Gap <- files[i - 1].Gap + cur.Size + cur.Gap
                files.RemoveAt(i)
                
                // Insert a file between gaps
                let fileBefore = files[indexBefore]
                files.Insert(indexBefore + 1, cur)
                let gap = fileBefore.Gap
                fileBefore.Gap <- 0
                cur.Gap <- gap - cur.Size
                
                // Mark as moved
                cur.moved <- true
            | _ ->
                i <- i - 1

    checksum files |> printfn "%d"
    
    // render2 files
