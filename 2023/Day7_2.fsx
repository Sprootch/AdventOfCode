open System.IO

type Card =
    | As
    | King
    | Queen
    | Joker
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
      Type: HandType }

let makeHand =
    function
    | 'A' -> As
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Joker
    | 'T' -> Number 10
    | n -> n |> string |> int |> Number

let cardStrength =
    function
    | As -> 14
    | King -> 13
    | Queen -> 12
    | Number n -> n
    | Joker -> 1

let handStrength =
    function
    | FiveOfAKind -> 15
    | FourOfAKind -> 14
    | FullHouse -> 13
    | ThreeOfAKind -> 12
    | TwoPair -> 11
    | OnePair -> 10
    | HighCard -> 9

let handType cards =
    match cards |> List.countBy id |> List.map snd |> List.sortDescending with
    | [ 5 ] -> FiveOfAKind
    | [ 4; _ ] -> FourOfAKind
    | [ 3; 2 ] -> FullHouse
    | [ 3; _; _ ] -> ThreeOfAKind
    | [ 2; 2; _ ] -> TwoPair
    | [ 2; _; _; _ ] -> OnePair
    | _ -> HighCard

let getType(cards: Card list) =
    let highestWithJoker =
        (As :: King :: Queen :: [ for n in 2..10 -> Number n ])
        |> List.map (fun card -> cards |> List.map (fun c -> if c = Joker then card else c))
        |> List.maxBy (fun cards -> cards |> handType |> handStrength)

    highestWithJoker |> handType

let createHand(line: string) =
    let splitted = line.Split(' ')
    let cards = splitted[0] |> Seq.map makeHand |> Seq.toList
    let bid = splitted[1] |> int
    let type' = cards |> getType

    { Bid = bid
      Cards = cards
      Type = type' }

let compareCard(card1, card2) =
    let cardStrength =
        function
        | As -> 14
        | King -> 13
        | Queen -> 12
        | Number n -> n
        | Joker -> 1

    // TODO: Change logic, Joker is weaker, not first.
    compare (card2 |> cardStrength) (card1 |> cardStrength)

let comparer hand1 hand2 =
    let handStrength =
        function
        | FiveOfAKind -> 6
        | FourOfAKind -> 5
        | FullHouse -> 4
        | ThreeOfAKind -> 3
        | TwoPair -> 2
        | OnePair -> 1
        | HighCard -> 0

    let strength1 = hand1.Type |> handStrength
    let strength2 = hand2.Type |> handStrength

    if (strength1 = strength2) then
        // TODO: Update this logic
        hand1.Cards
        |> List.zip hand2.Cards
        |> List.map compareCard
        |> List.find ((<>) 0)
    else if (strength1 > strength2) then
        1
    else
        -1

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

let input = File.ReadAllLines(Path.Combine("2023", "Input", "day7.txt"))
input |> Array.map createHand |> Array.sortWith comparer
