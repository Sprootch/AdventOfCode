module Tests

open System
open System.IO
open Xunit

module Day1 =
    [<Fact>]
    let ``Part 1 (example)``() =
        let example =
            "1abc2
                 pqr3stu8vwx
                 a1b2c3d4e5f
                 treb7uchet"
                .Split(Environment.NewLine)

        let result = Day1.count example
        Assert.Equal(142, result)

    [<Fact>]
    let ``Part 1``() =
        let txt = File.ReadLines(Path.Combine("Input", "day1.txt"))
        let result = Day1.count txt
        Assert.Equal(55488, result)

    [<Fact>]
    let ``Part 2 (example)``() =
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

        Assert.Equal(281, result)

module Day2 =
    let day2Example =
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

    [<Fact>]
    let ``Part 1 (example)``() =
        let example = day2Example.Split(Environment.NewLine)

        let result = Day2.solve example

        Assert.Equal(8, result)

    [<Fact>]
    let ``Part 1``() =
        let txt = File.ReadLines(Path.Combine("Input", "day2.txt"))

        let result = Day2.solve txt

        Assert.Equal(2285, result)

    [<Fact>]
    let ``Part 2 (example)``() =
        let example = day2Example.Split(Environment.NewLine)

        let result = Day2.solve2 example

        Assert.Equal(2286, result)

    [<Fact>]
    let ``Part 2``() =
        let txt = File.ReadLines(Path.Combine("Input", "day2.txt"))

        let result = Day2.solve2 txt

        Assert.Equal(77021, result)

module Day3 =
    let example =
        "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

    [<Fact>]
    let ``Part 1 (example)``() =
        let example = example.Split(Environment.NewLine)
        let result  = Day3.solve example
        
        Assert.Equal(4361, result)
