open System.IO

type Card =
    | As
    | King
    | Queen
    | Jack
    | Number of int

type HandType =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

type Hand =
    { Bid: int
      Cards: Card list
      Type: HandType
      Strength: int }

let makeHand =
    function
    | 'A' -> As
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Jack
    | 'T' -> Number 10
    | n -> n |> string |> int |> Number

let getType cards =
    // todo : take snd only?
    match cards |> List.countBy id |> List.sortByDescending snd with
    | [ (_, 5) ] -> FiveOfAKind
    | [ (_, 4); _ ] -> FourOfAKind
    | [ (_, 3); (_, 2) ] -> FullHouse
    | [ (_, 3); _; _ ] -> ThreeOfAKind
    | [ (_, 2); (_, 2); _ ] -> TwoPair
    | [ (_, 2); _; _; _ ] -> OnePair
    | _ -> HighCard

let getHandStrength =
    function
    | FiveOfAKind -> 6
    | FourOfAKind -> 5
    | FullHouse -> 4
    | ThreeOfAKind -> 3
    | TwoPair -> 2
    | OnePair -> 1
    | HighCard -> 0

let getCardStrength =
    function
    | As -> 14
    | King -> 13
    | Queen -> 12
    | Jack -> 11
    | Number n -> n
    
let createHand(line: string) =
    let splitted = line.Split(' ')
    let cards = splitted[0] |> Seq.map makeHand |> Seq.toList
    let bid = splitted[1] |> int
    let type' = cards |> getType
    let strength = type' |> getHandStrength

    { Bid = bid
      Cards = cards
      Strength = strength
      Type = type' }

let compareCard(card1, card2) =
    compare (card2 |> getCardStrength) (card1 |> getCardStrength)

let comparer hand1 hand2 =
    if (hand1.Strength > hand2.Strength) then
        1
    else if (hand2.Strength > hand1.Strength) then
        -1
    else
        hand1.Cards
        |> List.zip hand2.Cards
        |> List.map compareCard
        |> List.find ((<>) 0)

let example =
    """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""
        .Split("\n")

let solve input =
    input
    |> Array.map createHand
    |> Array.sortWith comparer
    |> Array.indexed
    // those 2 steps could be 1
    |> Array.map (fun (idx, el) -> (idx + 1) * el.Bid)
    |> Array.sum

// solve example

let input = File.ReadAllText(Path.Combine("2023", "Input", "day7.txt")).Split("\n")
solve input
