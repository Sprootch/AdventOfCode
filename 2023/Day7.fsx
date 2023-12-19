let input =
    """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

type Card =
    | As
    | King
    | Queen
    | Jack
    | Number of int

type HandType =
    | FiveOfAKind of Card
    | FourOfAKind of Card
    | FullHouse of Card*Card
    | ThreeOfAKind of Card
    | TwoPair of Card*Card
    | OnePair of Card
    | HighCard of Card

type Hand = Card array

let makeHand =
    function
    | 'A' -> As
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Jack
    | 'T' -> Number 10
    | c -> c |> string |> int |> Number

let getType hand : HandType =
    match hand with
    | [| (card, 5) |] -> FiveOfAKind card
    | [| (card, 4); _ |] -> FourOfAKind card
    | [| (three, 3); (two, 2) |] -> FullHouse (three,two)
    | [| (three, 3); _ |] -> ThreeOfAKind three
    | [| (p1, 2); (p2, 2) |] -> TwoPair (p1, p2)
    | [| (pair, 2); _ |] -> OnePair pair
    | card -> HighCard (Number 2) // TODO

let createHand(line: string) =
    let cards = line.Split(' ')[0] |> Seq.map makeHand |> Seq.toArray
    let bid = line.Split(' ')[1] |> int

    (cards |> Array.countBy id |> Array.sortByDescending snd)

let x(hand: (Card * int) array) = getType hand

let hands = input.Split("\n") |> Array.map createHand
hands |> Array.map getType


let test = [| (Queen, 3); (Jack, 1); (As, 1) |] 
test |> getType
