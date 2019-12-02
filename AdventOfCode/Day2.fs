module Day2
open Extensions

let deserialize (input : string) = input |> String.split ',' |> Seq.map int |> Seq.toList
let serialize (input : int list) = input |> Seq.map string |> String.join ","

let run (program : int list) : int list =
    let apply index func program =
        match program |> List.skip (index + 1) |> List.take 3 with
        | [ op1Index; op2Index; outputIndex ] -> 
            program |> List.replaceAt outputIndex (func program.[op1Index] program.[op2Index])
    
    let next index = index + 4

    let rec step (index : int) (program : int list) : int list =
        let instruction = program.[index]
        match instruction with
        | 1 -> program |> apply index (+) |> step (next index)
        | 2 -> program |> apply index (*) |> step (next index)
        | _ -> program

    program |> step 0

let runWithInputs a b input =
    input
    |> List.replaceAt 1 a
    |> List.replaceAt 2 b
    |> run
    |> List.head

let findArgumentsFor (needle : int) (program : int list) : (int * int) seq =
    Seq.allPairs (Seq.init 99 id) (Seq.init 99 id)
    |> Seq.filter (fun (a, b) -> (runWithInputs a b program) = needle)
    
