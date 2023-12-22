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
    | c -> c |> string |> int |> Number

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


let rec isPlusFort cards =
    match cards with
    | [] -> false
    | (c1, c2) :: t ->
        let s1 = c1 |> cardStrength
        let s2 = c2 |> cardStrength
        if (s1 = s2) then (isPlusFort t) else s1 > s2

let rec isMoinsFort cards =
    match cards with
    | [] -> false
    | (c1, c2) :: t ->
        let s1 = c1 |> cardStrength
        let s2 = c2 |> cardStrength
        if (s1 = s2) then (isMoinsFort t) else s1 < s2
        
let isGreater card1 card2 =
    if card1.Strength > card2.Strength then
        true
    else if card2.Strength > card1.Strength then
        false
    else
        card1.Cards |> List.zip card2.Cards |> isPlusFort

let isLowerOrEqual card1 card2 =
    if card1.Strength < card2.Strength then
        true
    else if card2.Strength > card1.Strength then
        false
    else
        card1.Cards |> List.zip card2.Cards |> isMoinsFort
        
let rec quicksort list =
    match list with
    | [] -> []
    | h :: t ->
        let lesser = t |> List.filter (isGreater h)
        let greater = t |> List.filter (isLowerOrEqual h)
        (quicksort lesser) @ [ h ] @ (quicksort greater)

let hands = input.Split("\n") |> Array.map createHand |> Array.toList //|> Array.sortByDescending _.Strength |> Array.toList

hands |> quicksort
