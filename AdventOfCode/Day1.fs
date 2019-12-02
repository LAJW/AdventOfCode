module Day1

let fuelNeeded mass = mass / 3 - 2

let rec fuelCost fuel =
    let extra = fuelNeeded fuel 
    if extra < 0 then 0
    else extra + (fuelCost extra)

let addFuelCost fuel =
    fuel + fuelCost fuel

let solve (input : string seq) =
    input |> Seq.sumBy (int >> fuelNeeded >> addFuelCost)
