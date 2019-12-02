namespace Test.Day2
open Microsoft.VisualStudio.TestTools.UnitTesting
open Test.Utils

[<TestClass>]
type Run() = 
    let func (input : string) =
        input |> Day2.deserialize |> Day2.run |> Day2.serialize

    [<TestMethod>]
    member this.TestAdd() =
        func "1,0,0,0,99" |> Assert.equal "2,0,0,0,99" // 1 + 1 = 2
    [<TestMethod>]
    member this.TestMultiplyOverwrite() =
        func "2,3,0,3,99" |> Assert.equal "2,3,0,6,99" // 3 * 2 = 6.
    [<TestMethod>]
    member this.TestMultiply() =
        func "2,4,4,5,99,0" |> Assert.equal "2,4,4,5,99,9801" // 99 * 99 = 9801.
    [<TestMethod>]
    member this.TestAll() =
        func "1,1,1,4,99,5,6,0,99" |> Assert.equal "30,1,1,4,2,5,6,0,99"

