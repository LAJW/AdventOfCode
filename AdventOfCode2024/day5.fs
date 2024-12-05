module AdventOfCode2024.day5

open FSharpPlus
open System.IO

let run1 () =
    let lines = File.ReadAllText("data5.txt")
    let [| top; bottom |] = lines.Split("\n\n")

    let ordering =
        top.Split("\n")
        |> Seq.map (String.split [ "|" ] >> Seq.map int >> toList)
        |> Seq.groupBy Seq.head
        |> Map
        |> Map.mapValues (Seq.map Seq.last >> Seq.toList)

    let updates =
        bottom.Split("\n")
        |> Seq.filter ((<>) "")
        |> Seq.map (String.split [ "," ] >> Seq.map int >> toList)
        |> toList


    let inRightOrder (update: int list) =
        seq { 0 .. update.Length - 1 }
        |> Seq.forall (fun i ->
            let first = update[i]
            let rest = update[i + 1 ..]
            let order = ordering.TryFind(first) |> Option.defaultValue []
            Set.isSubset (Set rest) (Set order))

    let middleNumber (update: int list) = update[update.Length / 2]

    let result = updates |> Seq.filter inRightOrder |> Seq.map middleNumber |> Seq.sum

    printfn "%d" result

let run2 () =
    let lines = File.ReadAllText("data5.txt")
    let [| top; bottom |] = lines.Split("\n\n")

    let ordering =
        top.Split("\n")
        |> Seq.map (String.split [ "|" ] >> Seq.map int >> toList)
        |> Seq.groupBy Seq.head
        |> Map
        |> Map.mapValues (Seq.map Seq.last >> Seq.toList)

    let updates =
        bottom.Split("\n")
        |> Seq.filter ((<>) "")
        |> Seq.map (String.split [ "," ] >> Seq.map int >> toList)
        |> toList


    let inRightOrder (update: int list) =
        seq { 0 .. update.Length - 1 }
        |> Seq.forall (fun i ->
            let first = update[i]
            let rest = update[i + 1 ..]
            let order = ordering.TryFind(first) |> Option.defaultValue []
            Set.isSubset (Set rest) (Set order))

    let middleNumber (update: int list) = update[update.Length / 2]

    let result =
        updates
        |> Seq.filter (not << inRightOrder)
        |> Seq.map (fun x ->
            x
            |> List.sortWith (fun a b ->
                if ordering.TryFind(a) |> Option.defaultValue [] |> List.contains b then
                    -1
                else
                    1))
        |> Seq.map middleNumber
        |> Seq.sum

    printfn "%d" result
