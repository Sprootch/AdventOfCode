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
