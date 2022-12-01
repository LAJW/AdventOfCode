namespace Test.Day6
open Microsoft.VisualStudio.TestTools.UnitTesting
open Extensions
open Test.Utils

[<TestClass>]
type Day6Test() =
    let yousan = "COM)B\n\
                  B)C\n\
                  C)D\n\
                  D)E\n\
                  E)F\n\
                  B)G\n\
                  G)H\n\
                  D)I\n\
                  E)J\n\
                  J)K\n\
                  K)L\n\
                  K)YOU\n\
                  I)SAN"
    [<TestMethod>]
    member this.CountOrbits() = 
        let input = "COM)B\n\
                     B)C\n\
                     C)D\n\
                     D)E\n\
                     E)F\n\
                     B)G\n\
                     G)H\n\
                     D)I\n\
                     E)J\n\
                     J)K\n\
                     K)L"
        input |> String.split '\n' |> Day6.parse |> Day6.countOrbits |> Assert.equal 42

    [<TestMethod>]
    member this.Path1() =
        let actual = yousan |> String.split '\n' |> Day6.parse |> Day6.path "YOU"
        let expected = ["COM"; "B"; "C"; "D"; "E"; "J"; "K"]
        CollectionAssert.AreEqual(List.toArray expected, List.toArray actual)

    [<TestMethod>]
    member this.Path2() =
        let actual = yousan |> String.split '\n' |> Day6.parse |> Day6.path "SAN"
        let expected = ["COM"; "B"; "C"; "D"; "I"]
        CollectionAssert.AreEqual(List.toArray expected, List.toArray actual)

    [<TestMethod>]
    member this.Distance() = 
        yousan |> String.split '\n' |> Day6.parse |> Day6.distance "YOU" "SAN" |> Assert.equal 4
