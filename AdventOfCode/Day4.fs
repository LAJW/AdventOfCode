module Day4

let hasAdjacentPair = Seq.pairwise >> Seq.exists (fun (a, b) -> a = b)

let neverDecreases = Seq.pairwise >> Seq.forall (fun (a, b) -> a <= b)

let nonAdjacentPairExists =
    Seq.pairwise
    >> Seq.filter (fun (a, b) -> a = b)
    >> Seq.countBy fst
    >> Seq.exists (snd >> ((=) 1)) // 1 - pair exists, 2 is too much

let isValid (password : string) =
    hasAdjacentPair password && neverDecreases password

let isValid2 (password : string) =
    neverDecreases password && nonAdjacentPairExists password

let findAllValidInRage validity min max =
    seq { for i in min .. max do yield i }
    |> Seq.filter (string >> validity)
    |> Seq.length
