type Card =
    | As
    | King
    | Queen
    | Jack
    | Number of int

let cardStrength =
    function
    | As -> 14
    | King -> 13
    | Queen -> 12
    | Jack -> 11
    | Number n -> n

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

let getType(cards: Card list) : HandType =
    match cards |> List.countBy id |> List.sortByDescending snd with
    | [ (_, 5) ] -> FiveOfAKind
    | [ (_, 4); _ ] -> FourOfAKind
    | [ (_, 3); (_, 2) ] -> FullHouse
    | [ (_, 3); _; _ ] -> ThreeOfAKind
    | [ (_, 2); (_, 2); _ ] -> TwoPair
    | [ (_, 2); _; _; _ ] -> OnePair
    | _ -> HighCard

let getStrength =
    function
    | FiveOfAKind -> 6
    | FourOfAKind -> 5
    | FullHouse -> 4
    | ThreeOfAKind -> 3
    | TwoPair -> 2
    | OnePair -> 1
    | HighCard -> 0

let createHand(line: string) =
    let cards = line.Split(' ')[0] |> Seq.map makeHand |> Seq.toList
    let bid = line.Split(' ')[1] |> int
    let type' = cards |> getType
    let strength = type' |> getStrength

    { Bid = bid
      Cards = cards
      Strength = strength
      Type = type' }

let input = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

let hands =
    input.Split("\n")
    |> Array.map createHand
    |> Array.sortByDescending _.Strength
    |> Array.toList

let compareCard(a, b) =
    let s1 = a |> cardStrength
    let s2 = b |> cardStrength
    compare s2 s1

let comparer a b =
    if (a.Strength > b.Strength) then
        1
    else if (b.Strength > a.Strength) then
        -1
    else
        a.Cards
        |> List.zip b.Cards
        |> List.map compareCard
        |> List.find (fun z -> z <> 0)

let sorted =
    hands
    |> List.sortWith comparer
    |> List.indexed
    |> List.map (fun (idx, el) -> (idx + 1) * el.Bid)
    |> List.sum
