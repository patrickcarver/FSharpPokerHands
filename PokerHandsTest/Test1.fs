namespace PokerHandsTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open PokerHands

[<TestClass>]
type Test1 () =
    [<TestMethod>]
    member _.``validateInputFile returns Error for empty args``() =
        match validateInputFile [||] with
        | Error error -> error |> should equal MissingFileArgument
        | Ok _ -> Assert.Fail "Expected Error, got Ok"

    [<TestMethod>]
    member _.``validateInputFile returns Error when file does not exist`` () =
        match validateInputFile [|"does_not_exist.txt"|] with
        | Error error -> error |> should equal (FileNotFound "does_not_exist.txt")
        | Ok _ -> Assert.Fail "Expected Error, got Ok"

    [<TestMethod>]
    member _.``validateInputFile returns Error when more than one item in args`` () =
        match validateInputFile [|"nope.txt"; "extra.txt"|] with
        | Error error -> error |> should equal TooManyArguments
        | Ok _ -> Assert.Fail "Expected Error, got Ok"
        
    [<TestMethod>]
    member _.``validateInputFile returns Ok fileName`` () =
        match validateInputFile [|"test.txt"|] with
        | Ok fileName -> fileName |> should equal "test.txt"
        | Error message -> Assert.Fail $"Expected Ok, got Error {message}"