module Day7

let findHighestOutput (program : int64 list) : int64 =
    let run a b =
        program |> Day5.run [a; b] |> List.head

    let runAllModules a b c d e =
        run a 0L
        |> run b
        |> run c
        |> run d
        |> run e

    let digits = seq { for i in 0..4 do yield i } |> Seq.map int64 |> Seq.toList

    let rec allPermutations (list : 'u list) (count : int) : 'u list seq =
        assert(count >= 1)
        if count = 1 then
            list |> Seq.map (fun x -> [x])
        else Seq.allPairs list (allPermutations list (count - 1))
            |> Seq.map (fun (a, bs) -> a :: bs)

    allPermutations digits 5
    |> Seq.filter (Set >> Set.count >> (=) 5)
    |> Seq.map (function [a; b; c; d; e] -> runAllModules a b c d e)
    |> Seq.max

