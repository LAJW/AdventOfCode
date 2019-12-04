module Day4

let pairs password = password |> Seq.map int |> Seq.pairwise

let hasAdjacentPair (password : string) =
    password |> pairs |> Seq.exists (fun (a, b) -> a = b)

let neverDecreases (password : string) =
    password |> pairs |> Seq.forall (fun (a, b) -> a <= b)

let nonAdjacentPairExists (password : string) =
    password
    |> pairs
    |> Seq.filter (fun (a, b) -> a = b)
    |> Seq.map snd
    |> Seq.groupBy id
    |> Seq.toList
    |> Seq.exists (snd >> Seq.length >> ((=) 1)) // 1 - pair exists, 2 is too much

let isValid (password : string) =
    hasAdjacentPair password && neverDecreases password

let isValid2 (password : string) =
    neverDecreases password && nonAdjacentPairExists password

let findAllValidInRage validity min max =
    seq { for i in min .. max do yield i }
    |> Seq.filter (string >> validity)
    |> Seq.length
