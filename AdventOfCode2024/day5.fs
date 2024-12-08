module AdventOfCode2024.day5

open FSharpPlus
open System.IO

let orderingAndUpdates () =
    let lines = File.ReadAllText("data5.txt")
    let [| top; bottom |] = lines.Split("\n\n")

    let ordering =
        top.Split("\n")
        |> Seq.map (String.split [ "|" ] >> Seq.map int)
        |> Seq.groupBy head
        |> Map
        |> Map.mapValues (Seq.map Seq.last >> toList)

    let updates =
        bottom.Split("\n")
        |> Seq.filter ((<>) "")
        |> Seq.map (String.split [ "," ] >> Seq.map int >> toList)
        |> toList

    ordering, updates

let inRightOrder (ordering: Map<int, int list>) (update: int list) =
    seq { 0 .. update.Length - 1 }
    |> Seq.forall (fun i ->
        let first = update[i]
        let rest = update[i + 1 ..]
        let order = ordering.TryFind(first) |> Option.defaultValue []
        Set.isSubset (Set rest) (Set order))

let middleNumber (update: int list) = update[update.Length / 2]

let run1 () =
    let ordering, updates = orderingAndUpdates ()
    let inRightOrder = inRightOrder ordering

    updates
    |> Seq.filter inRightOrder
    |> Seq.map middleNumber
    |> Seq.sum
    |> printfn "%d"

let run2 () =
    let ordering, updates = orderingAndUpdates ()
    let inRightOrder = inRightOrder ordering

    updates
    |> Seq.filter (not << inRightOrder)
    |> Seq.map (
        List.sortWith (fun a b ->
            if ordering.TryFind(a) |> Option.defaultValue [] |> List.contains b then
                -1
            else
                1)
    )
    |> Seq.map middleNumber
    |> Seq.sum
    |> printfn "%d"
