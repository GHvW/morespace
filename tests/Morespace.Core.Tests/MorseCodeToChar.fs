module Morespace.Core.Tests.MorseCodeToChar

open System
open Xunit
open FsUnit.Xunit

open Morespace.Core

[<Fact>]
let ``My test`` () =
    Assert.True(true)


type ``Given a sequence of dots and dashes`` () =

    [<Fact>]
    member it.``when the sequence is a valid morse code character`` () =
        let dds = "...-"
        let result = Parsers.morseCharacter dds
        result |> should equal 'v'