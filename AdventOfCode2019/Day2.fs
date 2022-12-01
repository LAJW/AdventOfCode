module Day2
open Extensions

let deserialize = Day5.deserialize
let serialize = Day5.serialize

let run (program : int64 list) : int64 list =
    program
    |> Day5.runFull []
    |> (fun state -> state.Program)

let runWithInputs a b =
    (function | head::_::_::rest -> head::a::b::rest)
    >> run
    >> List.head

let findArgumentsFor (needle : int64) (program : int64 list) : (int64 * int64) seq =
    Seq.allPairs (Seq.init 99 int64) (Seq.init 99 int64)
    |> Seq.filter (fun (a, b) -> (runWithInputs a b program) = needle)
    
