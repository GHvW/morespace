module Morespace.Core.Tests.MorseCodeToChar

open System
open Xunit
open FsUnit.Xunit

open Morespace.Core

[<Fact>]
let ``My test`` () = Assert.True(true)


type ``Given a sequence of dots and dashes``() =

    let dds =
        ".... . .-.. .-.. ---\t.-- --- .-. .-.. -.." // hello world

    [<Fact>]
    member it.``when parsing a single character``() =
        let struct (result, _) = Option.get (Parser.morseCharacter dds)
        result |> should equal 'h'

    [<Fact>]
    member it.``when parsing a word``() =

        let struct (result, _) = Option.get (Parser.morseWord dds)
        result |> should equal "hello"

    [<Fact>]
    member it.``when parsing a sentence``() =
        let struct (result, rest) = Option.get (Parser.morseCode dds)
        result |> should equal "hello world"
        rest |> should equal ""
