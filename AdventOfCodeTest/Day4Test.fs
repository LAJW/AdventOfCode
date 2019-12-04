namespace Test.Day4
open Microsoft.VisualStudio.TestTools.UnitTesting
open Test.Utils

[<TestClass>]
type IsValid() = 

    [<TestMethod>]
    member this.Example1() =
        "111111" |> Day4.isValid |> Assert.IsTrue

    [<TestMethod>]
    member this.Example2() =
        "223450" |> Day4.isValid |> Assert.IsFalse

    [<TestMethod>]
    member this.Example3() =
        "123789" |> Day4.isValid |> Assert.IsFalse
        
[<TestClass>]
type IsValid2() =
    [<TestMethod>]
    member this.Example1() =
        "112233" |> Day4.isValid2 |> Assert.IsTrue

    [<TestMethod>]
    member this.Example2() =
        "123444" |> Day4.isValid2 |> Assert.IsFalse

    [<TestMethod>]
    member this.Example3() =
        "111122" |> Day4.isValid2 |> Assert.IsTrue

