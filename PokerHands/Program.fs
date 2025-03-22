module PokerHands

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

type PokerError =
| MissingFileArgument
| TooManyArguments
| FileNotFound of string

let parseCardValue (rawValue: char)  = 
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

/// <summary>
/// Determines the winner of a round of Poker
/// </summary>
/// <param name="line">A space-delimited string that represents 10 cards; 
/// first 5 belong to Player One, the last 5 to Player Two</param>
/// <returns>The winning Player of this round of Poker</returns>
let winnerOfRound (line: string) : Player =
    let cardTokens = line.Split " " |> Array.toList
    let handOne = createHand cardTokens[0..4]
    let handTwo = createHand cardTokens[5..9]

    match (handOne, handTwo) with
    | (h1, h2) when h1 > h2 -> PlayerOne
    | (h1, h2) when h1 < h2 -> PlayerTwo
    | (h1, h2) when h1 = h2 -> failwith "Two hands should not be tied"
    | _ -> failwith "Unreachable: two hands should return a result with >, <, or =."

/// <summary>
/// Counts the wins of the specified player
/// </summary>
/// <param name="player">The player who's wins to count</param>
/// <param name="lines">A Seq that each item represent a round of Poker</param>
/// <returns>An int that is the player's total wins from the Seq provided</returns>
let countWins player lines =
    lines
    |> Seq.map winnerOfRound
    |> Seq.filter (fun p -> p = player)
    |> Seq.length

/// <summary>
/// Validates if there is a file name given and that if it exists
/// </summary>
/// <param name="args">The array that contains the file name; should be only one item</param>
/// <returns>A Result that's either Ok fileName or Error</return>
let validateInputFile (args: string array) : Result<string, PokerError> =
    match args with
    | [||] -> 
        Error MissingFileArgument 
    | [|fileName|] when not (File.Exists fileName) -> 
        Error (FileNotFound fileName)
    | [|fileName|] -> 
        Ok fileName
    | _ -> 
        Error TooManyArguments

/// <summary>
/// Prints the total wins of Player One given a formatted text file
/// </summary>
/// <param name="args">The command line parameter array</param>
[<EntryPoint>]
let main args =
    match validateInputFile args with
    | Ok fileName -> 
        File.ReadLines fileName 
        |> countWins PlayerOne 
        |> printfn "Player 1 wins: %i"
        0
    | Error error ->
        match error with
        | MissingFileArgument -> printfn "Missing input file name parameter."
        | TooManyArguments -> printfn "Too many arguments passed to command line."
        | FileNotFound fileName -> printfn "The file '%s' does not exist." fileName
        | _ -> printfn "Error catch all"
        1