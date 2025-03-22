namespace PokerHandsTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open PokerHands

[<TestClass>]
type TokenizeCardsTests () =
    [<DataTestMethod>]
    [<DataRow("8C TS KC 9H 4S 7D 2S 5D 3S AC")>]
    [<DataRow("8c ts kc 9h 4s 7d 2s 5d 3s ac")>]
    [<DataRow("8C   TS   KC     9H      4S      7D  2S   5D   3S AC")>]
    member _.``Ok line when it parses a valid line`` (line: string) =
        match tokenizeToCards line with
        | Ok cards -> cards |> should equal ["8C"; "TS"; "KC"; "9H"; "4S"; "7D"; "2S"; "5D"; "3S"; "AC"]
        | Error _ -> Assert.Fail "Expected Ok, got Error"

    [<DataTestMethod>]
    [<DataRow("")>]
    [<DataRow("blah")>]
    [<DataRow("8X 0S KC 9H 4S 7G 2W 5Q 3P AC")>]
    member _.``Error InvalidLine line when invalid line`` (line: string) =
        match tokenizeToCards line with
        | Error error -> error |> should equal (InvalidLine line)
        | Ok cards -> Assert.Fail $"Expected Error InvalidLine {line}, got Ok {cards}"

    [<DataTestMethod>]
    [<DataRow("8C 8C KC 9H 4S 7D 2S 5D 3S AC")>]
    [<DataRow("8C 8C 8C 8C 8C 8C 8C 8C 8C 8C")>]
    member _.``Error DuplicateCards when duplicated cards in line`` (line: string) =
        match tokenizeToCards line with
        | Error error -> error |> should equal (DuplicateCards line)
        | Ok cards -> Assert.Fail $"Expected Error DuplicateCards {line}, got Ok {cards}"

