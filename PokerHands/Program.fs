open System
open System.IO

type Player = PlayerOne | PlayerTwo

type Rank = int

type Hand =
| RoyalFlush
| StraightFlush of rank: Rank
| FourOfAKind of rank: Rank
| FullHouse of rank: Rank
| Straight of rank: Rank
| Flush of cards: Rank list
| ThreeOfAKind of rank: Rank
| TwoPairs of highPair: Rank * lowPair: Rank * kicker: Rank
| OnePair of pair: Rank * kickers: Rank list
| HighCard of cards: Rank list

let rankToInt rank =
    match rank with
    | RoyalFlush -> 10
    | StraightFlush _ -> 9
    | FourOfAKind _ -> 8
    | FullHouse _ -> 7
    | Straight _ -> 6
    | Flush _ -> 5
    | ThreeOfAKind _ -> 4
    | TwoPairs _ -> 3
    | OnePair _ -> 2
    | HighCard _ -> 1

let parseCardValue rawValue = 
    match rawValue with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c when c >= '2' && c <= '9' -> int (string c) 
    | _ -> failwith $"Invalid value {rawValue}"   

let parseCardTokens (cardTokens: string list) =
    cardTokens 
    |> List.map(fun ct -> (parseCardValue ct.[0], ct.[1])) 
    |> List.unzip

let groupByCount (values: int list) =
    let group acc (value, counts) =
        acc |> Map.change counts (fun current -> 
            match current with
            | None -> Some [value]
            | Some existing -> Some (existing @ [value]))

    values 
    |> List.countBy id 
    |> List.fold group Map.empty

let evaluateMultiples (values: int list) =
    let groups = groupByCount values

    let hasFourOfAKind = Map.containsKey 4 groups
    let hasThreeOfAKind = Map.containsKey 3 groups
    let numPairs = 
        if Map.containsKey 2 groups then
            List.length groups.[2]
        else
            0

    match (hasThreeOfAKind, numPairs) with
    | (true, 0) -> ThreeOfAKind groups.[3].Head
    | (true, 1) -> FullHouse groups.[3].Head
    | (false, 1) -> OnePair (pair = groups.[2].Head, kickers = List.sortDescending groups.[1])
    | (false, 2) -> TwoPairs (highPair = List.head groups.[2], lowPair = List.last groups.[2], kicker = List.head groups.[1])
    | (false, 0) -> 
        if hasFourOfAKind then
            FourOfAKind groups.[4].Head
        else
            HighCard groups.[1]
    | _ -> failwith $"This number of pairs: {numPairs} is impossible"

let evaluateSequenceAndSuits (values: int list) (suits: char list) =
    let isFlush = suits |> List.distinct |> List.length = 1
    
    let sortedValues = List.sortByDescending id values
    let isStraight = (sortedValues = [sortedValues.[0] .. -1 .. sortedValues.[4]])

    match (isFlush, isStraight) with
    | (false, false) -> HighCard sortedValues
    | (true, false) -> Flush sortedValues
    | (false, true) -> Straight sortedValues.Head
    | (true, true) -> 
        match sortedValues with
        | [14; 13; 12; 11; 10] -> RoyalFlush
        | _ -> StraightFlush sortedValues.Head
    
let handCreate (cardTokens: string list) =
    let (values, suits) = parseCardTokens cardTokens
    let multipleHand = evaluateMultiples values

    match multipleHand with 
    | HighCard _ -> evaluateSequenceAndSuits values suits
    | other -> other

let breakRankTie handOne handTwo = 
    match (handOne, handTwo) with
    | (StraightFlush one, StraightFlush two)
    | (FourOfAKind one, FourOfAKind two)
    | (FullHouse one, FullHouse two)
    | (Straight one, Straight two)
    | (ThreeOfAKind one, ThreeOfAKind two) -> 
        compare one two
    | (Flush cardsOne, Flush cardsTwo)
    | (HighCard cardsOne, HighCard cardsTwo) -> 
        compare cardsOne cardsTwo
    | (TwoPairs (highPairOne, lowPairOne, kickerOne), TwoPairs (highPairTwo, lowPairTwo, kickerTwo)) ->
        compare [highPairOne; lowPairOne; kickerOne] [highPairTwo; lowPairTwo; kickerTwo]        
    | (OnePair (pairOne, kickersOne), OnePair (pairTwo, kickersTwo)) ->
        compare (pairOne :: kickersOne) (pairTwo :: kickersTwo)
    | (RoyalFlush, RoyalFlush) -> 0
    | _ -> failwith $"Both hands need to have the same rank in order to break a rank tie."

let winnerOfRound (line: string) =
    let cardTokens = line.Split " " |> Array.toList
    let handOne = handCreate(cardTokens[0..4])
    let handTwo = handCreate(cardTokens[5..9])

    let rankResult = compare (rankToInt handOne) (rankToInt handTwo) 
    match rankResult with
    | 1 -> PlayerOne
    | -1 -> PlayerTwo
    | 0 -> 
        match breakRankTie handOne handTwo with
        | 1 -> PlayerOne
        | -1 -> PlayerTwo
        | 0 -> failwith $"The two hands have the same rank and same value of cards; this is a tie which is not supposed to happen"
        | _ -> failwith $""
    | _ -> failwith $"The result of rank compare resulted in an impossible value: {rankResult}"
    
let countWins player lines =
    lines
    |> Seq.map winnerOfRound
    |> Seq.filter (fun p -> p = player)
    |> Seq.length

let readLines (filePath:string) = 
    seq { use reader = new StreamReader(filePath) 
        while not reader.EndOfStream do
            yield reader.ReadLine() }

[<EntryPoint>]
let main args =
    readLines args.[0] 
    |> countWins PlayerOne 
    |> printfn "Player 1 wins: %i"

    0