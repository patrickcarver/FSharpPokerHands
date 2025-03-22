namespace PokerHandsTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open PokerHands

[<TestClass>]
type ValidateInputFileTests () =
    [<TestMethod>]
    member _.``Error for empty args``() =
        match validateInputFile [||] with
        | Error error -> error |> should equal MissingFileArgument
        | Ok _ -> Assert.Fail "Expected Error, got Ok"

    [<TestMethod>]
    member _.``Error when file does not exist`` () =
        let fileName = "does_not_exist.txt"
        match validateInputFile [|fileName|] with
        | Error error -> error |> should equal (FileNotFound fileName)
        | Ok _ -> Assert.Fail "Expected Error, got Ok"

    [<TestMethod>]
    member _.``Error when more than one item in args`` () =
        match validateInputFile [|"nope.txt"; "extra.txt"|] with
        | Error error -> error |> should equal TooManyArguments
        | Ok _ -> Assert.Fail "Expected Error, got Ok"
        
    [<TestMethod>]
    member _.``Ok fileName`` () =
        match validateInputFile [|"test.txt"|] with
        | Ok fileName -> fileName |> should equal "test.txt"
        | Error message -> Assert.Fail $"Expected Ok, got Error {message}"