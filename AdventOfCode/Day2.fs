module Day2
open System

type String with
    static member join (separator : string) (values : string seq) = String.Join(separator, values)

type List<'u> with
    static member replaceAt index newValue list =
        list |> List.mapi(fun i value -> if i = index then newValue else value)

let deserialize (input : string) = input.Split(',') |> Seq.map int |> Seq.toList
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

let func2 (input : string) =
    let program = deserialize input
    let permutations = Seq.allPairs (Seq.init 99 id) (Seq.init 99 id)
    permutations
    |> Seq.filter (fun (a, b) -> (runWithInputs a b program) = 19690720)
    |> Seq.map(fun tuple -> tuple.ToString())
    |> String.join "|"

let func (input : string) =
    input
    |> deserialize
    |> run
    |> serialize
    

