module Day2
open Extensions

let deserialize = String.split ',' >> Seq.map int >> Seq.toList
let serialize (input : int list) = input |> Seq.map string |> String.join ","

let run (program : int list) : int list = (Day5.runFull [] program).Program

let runWithInputs a b =
    (function | head::_::_::rest -> head::a::b::rest)
    >> run
    >> List.head

let findArgumentsFor (needle : int) (program : int list) : (int * int) seq =
    Seq.allPairs (Seq.init 99 id) (Seq.init 99 id)
    |> Seq.filter (fun (a, b) -> (runWithInputs a b program) = needle)
    
