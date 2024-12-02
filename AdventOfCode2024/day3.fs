module AdventOfCode2024.day3

let run1() =
    let lines = File.ReadAllLines "./data3.txt"

    let numbers = lines |> map(String.split [" "] >> Seq.map int >> toList)
    
    printfn "%d" result
