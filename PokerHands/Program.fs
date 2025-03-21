open System.IO

type Player = PlayerOne | PlayerTwo

type Rank = int

type Hand =
| HighCard of Rank list
| OnePair of pair: Rank * kickers: Rank list
| TwoPairs of highPair: Rank * lowPair: Rank * kicker: Rank
| ThreeOfAKind of Rank
| Flush of Rank list
| Straight of Rank
| FullHouse of Rank
| FourOfAKind of Rank
| StraightFlush of Rank
| RoyalFlush

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
            | Some existing -> Some (value :: existing))

    values 
    |> List.countBy id 
    |> List.fold group Map.empty

let tryNumPairs groups =
    groups 
    |> Map.tryFind 2 
    |> Option.map List.length 
    |> Option.defaultValue 0    

let evaluateMultiples (values: int list) : Hand =
    let groups = groupByCount values

    let four = groups |> Map.tryFind 4
    let three = groups |> Map.tryFind 3
    let ones = groups |> Map.tryFind 1 |> Option.defaultValue []
    let numPairs = tryNumPairs groups
    
    match (four, three, numPairs) with
    | (Some vals, _, _) -> FourOfAKind vals.Head
    | (None, Some vals, 0) -> ThreeOfAKind vals.Head
    | (None, Some vals, 1) -> FullHouse vals.Head
    | (None, None, 1) -> 
        OnePair (
            pair = groups.[2].Head, 
            kickers = List.sortDescending ones)
    | (None, None, 2) -> 
        TwoPairs (
            highPair = List.head groups.[2], 
            lowPair = List.last groups.[2], 
            kicker = List.head ones)
    | (None, None, 0) when not values.IsEmpty -> 
        HighCard (List.sortDescending ones)
    | _ -> failwith $"Invalid hand configuration: {values}"

let evaluateSequenceAndSuits (values: int list) (suits: char list) : Hand =
    let isFlush = suits |> List.distinct |> List.length = 1
    
    let sortedValues = List.sortByDescending id values
    let isStraight = (sortedValues = [sortedValues.[0] .. -1 .. sortedValues.[4]])

    match (isFlush, isStraight) with
    | (false, false) -> HighCard sortedValues
    | (true, false) -> Flush sortedValues
    | (false, true) -> Straight sortedValues.Head
    | (true, true) when sortedValues = [14; 13; 12; 11; 10] -> RoyalFlush 
    | (true, true) -> StraightFlush sortedValues.Head
    
let createHand (cardTokens: string list) : Hand =
    let (values, suits) = parseCardTokens cardTokens
    let multipleHand = evaluateMultiples values

    match multipleHand with 
    | HighCard _ -> evaluateSequenceAndSuits values suits
    | other -> other

let winnerOfRound (line: string) : Player =
    let cardTokens = line.Split " " |> Array.toList
    let handOne = createHand cardTokens[0..4]
    let handTwo = createHand cardTokens[5..9]

    match (handOne, handTwo) with
    | (h1, h2) when h1 > h2 -> PlayerOne
    | (h1, h2) when h1 < h2 -> PlayerTwo
    | (h1, h2) when h1 = h2 -> failwith "Two hands should not be tied"
    | _ -> failwith "Unreachable: two hands should return a result with >, <, or =."

let countWins player lines =
    lines
    |> Seq.map winnerOfRound
    |> Seq.filter (fun p -> p = player)
    |> Seq.length

let readLines (filePath:string) : string seq = 
    seq { use reader = new StreamReader(filePath) 
        while not reader.EndOfStream do
            yield reader.ReadLine() }

[<EntryPoint>]
let main args =
    if Array.isEmpty args then
        printfn "Missing input file name parameter."
        1
    else
        let fileName = args.[0]
        if not (File.Exists fileName) then
            printfn $"The file '{fileName}' does not exist"
            1
        else            
            readLines args.[0] 
            |> countWins PlayerOne 
            |> printfn "Player 1 wins: %i"
            0