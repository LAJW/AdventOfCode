namespace Test.Day10
open Microsoft.VisualStudio.TestTools.UnitTesting
open Test.Utils
open VecI

[<TestClass>]
type FindBestStation() =
    [<TestMethod>]
    member this.Map1() =
        let map = ".#..#\
                   .....\
                   #####\
                   ....#\
                   ...##"
        let parsed = Day10.parse 5 5 map
        parsed |> Day10.findBestStationLocation |> VecI.toPair |> Assert.equal (3, 4)

    [<TestMethod>]
    member this.Map2() =
        let map = "......#.#.\
                   #..#.#....\
                   ..#######.\
                   .#.#.###..\
                   .#..#.....\
                   ..#....#.#\
                   #..#....#.\
                   .##.#..###\
                   ##...#..#.\
                   .#....####"
        let parsed = Day10.parse 10 10 map
        parsed |> Day10.findBestStationLocation |> VecI.toPair |> Assert.equal (5, 8)

    [<TestMethod>]
    member this.Map3() =
        let map = "#.#...#.#.\
                   .###....#.\
                   .#....#...\
                   ##.#.#.#.#\
                   ....#.#.#.\
                   .##..###.#\
                   ..#...##..\
                   ..##....##\
                   ......#...\
                   .####.###."
        let parsed = Day10.parse 10 10 map
        parsed |> Day10.findBestStationLocation |> VecI.toPair |> Assert.equal (1, 2)

    [<TestMethod>]
    member this.Map4() =
        let map = ".#..#..###\
                   ####.###.#\
                   ....###.#.\
                   ..###.##.#\
                   ##.##.#.#.\
                   ....###..#\
                   ..#.#..#.#\
                   #..#.#.###\
                   .##...##.#\
                   .....#.#.."
        let parsed = Day10.parse 10 10 map
        parsed |> Day10.findBestStationLocation |> VecI.toPair |> Assert.equal (6, 3)
