module Morespace.Core.Tests.MorseCodeToChar

open System
open Xunit
open FsUnit.Xunit

open Morespace.Core

[<Fact>]
let ``My test`` () = Assert.True(true)


type ``Given a sequence of dots and dashes``() =

    [<Fact>]
    member it.``when parsing a single character``() =
        let dds = "...- "
        let struct (result, rest) = Option.get (Parser.morseCharacter dds)
        result |> should equal 'v'
        rest |> should equal " "

    [<Fact>]
    member it.``when parsing a word``() =
        let dds =
            ".... . .-.. .-.. ---\t.-- --- .-. .-.. .--" // hello world

        let struct (result, rest) = Option.get (Parser.word dds)
        result |> should equal "hello"
