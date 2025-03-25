module PokerHands

open System
open System.IO
open System.Text.RegularExpressions

/// Type for Player 1 or Player 2
type Player = PlayerOne | PlayerTwo

/// Type that represents values for cards
type CardValue = CardValue of int

/// Type that represents suits for cards
type Suit = Suit of char

/// Type that represents counts of card values
type CardCountMap = CardCountMap of Map<int, CardValue list>

/// Type that represents card tokens which are the strings 
/// after spliting a string line from the file.
type CardToken = CardToken of string

/// Type for a hand of Poker
type Hand =
| HighCard of CardValue list
| OnePair of pair: CardValue * kickers: CardValue list
| TwoPairs of highPair: CardValue * lowPair: CardValue * kicker: CardValue
| ThreeOfAKind of CardValue
| Flush of CardValue list
| Straight of CardValue
| FullHouse of CardValue
| FourOfAKind of CardValue
| StraightFlush of CardValue
| RoyalFlush

/// Errors used for input issues.
type InputFileError =
| MissingFileArgument
| TooManyArguments
| FileNotFound of string

/// <summary>
/// A list of CardValues used to check if another list of CardValues is a RoyalFlush
/// </summary>
let RoyalFlushValues : CardValue list = 
    [CardValue 14; CardValue 13; CardValue 12; CardValue 11; CardValue 10]

/// <summary>
/// Transform the char card value to an int
/// </summary>
/// <param name="rawValue">Char value of a card</param>
/// <returns></returns>
let parseCardValue (rawValue: char) : CardValue = 
    match rawValue with
    | 'A' -> CardValue 14
    | 'K' -> CardValue 13
    | 'Q' -> CardValue 12
    | 'J' -> CardValue 11
    | 'T' -> CardValue 10
    | c when c >= '2' && c <= '9' -> CardValue (int (string c)) 
    | _ -> failwith $"Invalid value {rawValue}"   

/// <summary>
/// Separate card values and suits into two lists
/// </summary>
/// <param name="cardTokens">A list of strings that represent cards</param>
let parseCardTokens (cardTokens: CardToken list) : CardValue list * Suit list =
    cardTokens 
    |> List.map(fun (CardToken ct) -> (parseCardValue ct.[0], Suit ct.[1])) 
    |> List.unzip

/// <summary>
/// Adds or updates a key which if the card count and a list of card values
/// </summary>
/// <param name="acc">The map used to store the counts for cards.</param>
/// <param name="value">The card value.</param>
/// <param name="counts">The total count of cards for a value.</param>
/// <returns>A map with the key </returns>
let addToCardCount (acc: Map<int, CardValue list>) (value: CardValue, counts: int) : Map<int, CardValue list> =
    acc 
    |> Map.change counts (fun current -> 
        match current with
        | None -> Some [value]
        | Some existing -> Some (value :: existing))

/// <summary>
/// Returns a Map of the count of each value in a list of cards
/// </summary>
/// <param name="values">Card values used for a hand</param>
/// <returns>A CardCountMap, the keys are the count of cards, the values are the card values</returns>
let groupByCardCount (values: CardValue list) : CardCountMap =
    values 
    |> List.countBy id 
    |> List.fold addToCardCount Map.empty
    |> CardCountMap

/// <summary>
/// Extract the list of card values with a count of 1
/// </summary>
/// <param name="cardCount">A Map of the counts of each card value for a hand.</param>
/// <returns>A sorted descending list of card values that have a count of 1 or an empty list.</returns>
let tryOnes (CardCountMap cardCount) =
    cardCount 
    |> Map.tryFind 1 
    |> Option.defaultValue [] 
    |> List.sortDescending    

/// <summary>
/// Find out the number of pairs to be used in a hand.
/// </summary>
/// <param name="cardCount">A Map of the counts of each card value for a hand</param>
/// <returns>An int of the number of card pairs.</returns>
let tryNumPairs (CardCountMap cardCount) : int =
    cardCount 
    |> Map.tryFind 2 
    |> Option.map List.length 
    |> Option.defaultValue 0    

/// <summary>
/// Tries to create a Hand that has multiples of same card values
/// </summary>
/// <description>
/// Tries to create One Pair, Two Pairs, Three of a Kind, Full House, or Four of a Kind.
/// If it cannot do that, a High Card hand is returned.
/// </description>
/// <param name="cardCount">Contains the card count as keys with a list of card values.</param>
/// <returns>
/// Either a hand that matches the criteria for having mulitples of the same card values or a High Card.
/// </returns>
let evaluateMultiples (CardCountMap cardCount) : Hand =
    let four = cardCount |> Map.tryFind 4
    let three = cardCount |> Map.tryFind 3
    let ones = tryOnes (CardCountMap cardCount)
    let numPairs = tryNumPairs (CardCountMap cardCount)
    
    match (four, three, numPairs) with
    | (Some vals, _, _) -> FourOfAKind vals.Head
    | (None, Some vals, 0) -> ThreeOfAKind vals.Head
    | (None, Some vals, 1) -> FullHouse vals.Head
    | (None, None, 1) -> 
        OnePair (
            pair = cardCount.[2].Head, 
            kickers = ones)
    | (None, None, 2) -> 
        TwoPairs (
            highPair = List.head cardCount.[2], 
            lowPair = List.last cardCount.[2], 
            kicker = List.head ones)
    | (None, None, 0) when not ones.IsEmpty -> 
        HighCard ones
    | _ -> failwith $"Invalid CardCountMap: {cardCount}"

/// <summary>
/// Tries to create a Hand that is either some flavor of straight or flush.
/// </summary>
/// <description>
/// Tries to create a Straight, Flush, Straight Flush, or Royal Flush
/// If it cannot do that, a High Card hand is returned.
/// </description>
/// <param name="values">An int list of card values for a hand</param>
/// <param name="suits">A char list of card suits for a hand</param>
/// <returns>
/// Either a hand that matches the critieria for a straight and/or flush or a High Card.
/// </returns>
let evaluateSequenceAndSuits (values: CardValue list) (suits: Suit list) : Hand =
    let createDescendingCardValues (CardValue start) (CardValue last) : CardValue list =
        [start .. -1 .. last] |> List.map CardValue

    let isFlush = suits |> List.distinct |> List.length = 1
    
    let sortedValues = List.sortDescending values
    let descendingCardValues =  createDescendingCardValues sortedValues.[0] sortedValues.[4]
    let isStraight = (sortedValues = descendingCardValues)

    match (isFlush, isStraight) with
    | (false, false) -> HighCard sortedValues
    | (true, false) -> Flush sortedValues
    | (false, true) -> Straight sortedValues.Head
    | (true, true) when sortedValues = RoyalFlushValues -> RoyalFlush 
    | (true, true) -> StraightFlush sortedValues.Head

/// <summary>
/// Create a ranked Poker hand from a list of card tokens
/// </summary>
/// <param name="cardTokens">card tokens</param>
/// <returns>A Poker hand that is ranked, e.g. a straight with the highest value of 10</returns>
let createHand (cardTokens: CardToken list) : Hand =
    let (values, suits) = parseCardTokens cardTokens

    let multipleHand = 
        values 
        |> groupByCardCount 
        |> evaluateMultiples 

    match multipleHand with 
    | HighCard _ -> evaluateSequenceAndSuits values suits
    | other -> other

/// <summary>
/// Valid and split a line into tokens of cards
/// </summary>
/// <param name="line">The string that represents the 10 cards of a two player Poker round</param>
/// <returns>A string list of card tokens.</returns>
/// <exception>Thrown if the line is not a valid format or if there are duplicate cards</exception>
let tokenizeToCards (line: string) : CardToken list =

    // This is the pattern used to validate a line:
    // A card should be two chars, the first is the value, the second is the suit.
    // Values must be a digit 2 through 9 or character T, J, Q, K, or A (Ten, Jack, Queen, King, Ace).
    // Suits must be character C, D, S, or H (Clubs, Diamonds, Spades, or Hearts).
    // Each card must be separated by at least one whitespace char.
    // There must be exactly 10 cards.    
    let pattern =  
        @"^([2-9TJQKA])([CDSH])(?:\s+([2-9TJQKA])([CDSH])){9}$"
    
    let upperLine = line.ToUpper();

    if Regex.IsMatch (upperLine, pattern) then
        let cardTokens = 
            upperLine.Split(" ", StringSplitOptions.RemoveEmptyEntries) 
            |> Array.toList 
            |> List.map CardToken
        
        let uniqueCardCount = 
            cardTokens 
            |> List.distinct 
            |> List.length
        
        match uniqueCardCount with
        | 10 -> cardTokens
        | _ -> failwith $"The line '{line}' has duplicate cards."
    
    else
        failwith $"The line '{line}' is not a valid format for a round of Poker."

/// <summary>
/// Determines the winner of a round of Poker
/// </summary>
/// <param name="line">A space-delimited string that represents 10 cards; 
/// first 5 belong to Player One, the last 5 to Player Two</param>
/// <returns>The winning Player of this round of Poker</returns>
let winnerOfRound (line: string) : Player =
    let cardTokens = tokenizeToCards line

    let handOne = createHand cardTokens[0..4]
    let handTwo = createHand cardTokens[5..9]

    match (handOne, handTwo) with
    | (h1, h2) when h1 > h2 -> PlayerOne
    | (h1, h2) when h1 < h2 -> PlayerTwo
    | (h1, h2) when h1 = h2 -> failwith "Two hands are tied which is not possible according to the requirements."
    | _ -> failwith "Unreachable: two hands should return a result with >, <, or =."
    
/// <summary>
/// Counts the wins of the specified player
/// </summary>
/// <param name="player">The player who's wins to count</param>
/// <param name="lines">A Seq that each item represent a round of Poker</param>
/// <returns>An int that is the player's total wins from the Seq provided</returns>
let countWins (player) (lines) : int =
    lines
    |> Seq.map winnerOfRound
    |> Seq.filter (fun p -> p = player)
    |> Seq.length

/// <summary>
/// Validates if there is a file name given and that if it exists
/// </summary>
/// <param name="args">The array that contains the file name; should be only one item</param>
/// <returns>A Result that's either Ok fileName or Error</returns>
let validateInputFile (args: string array) : Result<string, InputFileError> =
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
        try
            File.ReadLines fileName 
            |> countWins PlayerOne 
            |> printfn "Player 1 wins: %i" 
            0
        with
        | ex -> 
            printfn "Error: %s" ex.Message
            1
    | Error error ->
        match error with
        | MissingFileArgument -> printfn "Missing input file name parameter."
        | TooManyArguments -> printfn "Too many arguments passed to command line."
        | FileNotFound fileName -> printfn "The file '%s' does not exist." fileName
        1