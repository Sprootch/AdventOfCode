open System
open System.IO
open System.Text.RegularExpressions

let example =
    """two1nine
             eightwothree
             abcone2threexyz
             xtwone3four
             4nineeightseven2
             zoneight234
             7pqrstsixteen"""
        .Split("\n")

let processLine(str: string) =
    let digits = str |> Seq.filter Char.IsDigit
    let first = digits |> Seq.head
    let last = digits |> Seq.last
    $"{first}{last}" |> int

let replaceDigits(line: string) =
    let folder (str: string) (mat: Match) =
        let map =
            Map
                [ "one", "1"
                  "two", "2"
                  "three", "3"
                  "four", "4"
                  "five", "5"
                  "six", "6"
                  "seven", "7"
                  "eight", "8"
                  "nine", "9" ]

        str.Replace(mat.Value, map[mat.Value])

    let matches =
        Regex.Matches(line, "(one|two|three|four|five|six|seven|eight|nine)", RegexOptions.Multiline)

    matches |> Seq.fold folder line

let solve(input: string array) =
    input |> Array.map replaceDigits |> Array.map processLine |> Array.sum

let input = File.ReadAllLines(Path.Combine("2023", "Input", "day1.txt"))
// solve input

// let s = "fx3"
// solve [| s  |]

// solve example

// 55600 too low
