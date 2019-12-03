namespace Test.Day3
open Microsoft.VisualStudio.TestTools.UnitTesting
open Test.Utils

[<TestClass>]
type Distance() = 
    [<TestMethod>]
    member this.Distance0() =
        Day3.distance [
            "R8,U5,L5,D3"
            "U7,R6,D4,L4"
        ] |> Assert.equal 6
    [<TestMethod>]
    member this.Distance1() =
        Day3.distance [
            "R75,D30,R83,U83,L12,D49,R71,U7,L72"
            "U62,R66,U55,R34,D71,R55,D58,R83"
        ] |> Assert.equal 159
    [<TestMethod>]
    member this.Distance2() =
        Day3.distance [
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" 
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        ] |> Assert.equal 135
