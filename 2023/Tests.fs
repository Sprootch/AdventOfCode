module Tests

open System
open Xunit
open Xunit.Abstractions

type Tests(output: ITestOutputHelper) =
    let write result =
        output.WriteLine $"The actual result was {result}"

    [<Fact>]
    let ``Day 1-1 (example)``() =
        let example =
            "1abc2
                 pqr3stu8vwx
                 a1b2c3d4e5f
                 treb7uchet"
                .Split(Environment.NewLine)

        let result = Day1.count example
        Assert.Equal(142, result)

    [<Fact>]
    let ``Day 1-1``() =
        let txt = System.IO.File.ReadLines "Input\\day1.txt"
        let result = Day1.count txt
        Assert.Equal(55488, result)

    [<Fact>]
    let ``Day 1-2 (example)``() =
        let example =
            "two1nine
                 eightwothree
                 abcone2threexyz
                 xtwone3four
                 4nineeightseven2
                 zoneight234
                 7pqrstsixteen"
                .Split(Environment.NewLine)

        let result = Day1.count2 example
        write result

        Assert.Equal(281, result)

    [<Fact>]
    let ``Day 2-1 (example)``() =
        let example =
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
                .Split(Environment.NewLine)

        let result = Day2.solve example

        Assert.Equal(8, result)
        
    [<Fact>]
    let ``Day 2-1``() =
        let txt = System.IO.File.ReadLines "Input\\day2.txt"
        
        let result = Day2.solve txt

        Assert.Equal(2285, result)
