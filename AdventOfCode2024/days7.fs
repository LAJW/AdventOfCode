module AdventOfCode2024.daya7

// #r "nuget: FSharpPlus"
open System
open System.IO
open System.Linq.Expressions
open FSharpPlus

type Node =
    | Value of int64
    | Add
    | Mul

let rec increment (counter: Node list) =
    match counter with
    | [] -> []
    | Add :: tail -> Mul :: tail
    | Mul :: tail -> Add :: (increment tail)
    | s -> failwith ("Bad counter" + s.ToString())

let rec execute (expr: Node list) =
    match expr with
    | [ Value a ] -> [ Value a ]
    | Value a :: Add :: Value b :: rest -> Value(a + b) :: rest |> execute
    | Value a :: Mul :: Value b :: rest -> Value(a * b) :: rest |> execute
    | s -> failwith ("Bad expression" + s.ToString())

let run1 () =
    let lines = File.ReadAllLines("data7.txt")

    let count =
        lines
        |> Seq.map (fun line ->
            let [| controlStr; numbersStr |] = line.Split(": ")
            let control = int64 controlStr
            let numbers = numbersStr.Split(" ") |> Seq.map (int64 >> Value) |> toList
            let counter =
                seq { 0 .. numbers.Length - 2 } |> Seq.map (fun _ -> Add) |> Seq.toList

            let combinations =
                (counter, seq { 0 .. (pown 2 counter.Length) - 2 })
                ||> Seq.scan (fun state i -> increment state)
                |> toList

            let value =
                combinations
                |> Seq.exists (fun operators ->
                    let expression =
                        seq {
                            yield numbers[0]

                            for i in 1 .. numbers.Length - 1 do
                                yield operators[i - 1]
                                yield numbers[i]
                        }
                        |> toList

                    let result =
                        match execute expression with
                        | [ Value x ] -> x

                    result = control)
            if value then control else 0)
        |> Seq.sum

    printfn "%d" count
    ()
