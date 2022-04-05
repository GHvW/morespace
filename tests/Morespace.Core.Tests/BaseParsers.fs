module Morespace.Core.Tests.BaseParsers

open System
open Xunit
open FsUnit.Xunit

open Morespace.Core
open Morespace.Core.MorseCode

[<Fact>]
let ``alpha to morse lookup works correctly`` () =
    Alphabet.alphaToMorse.['v'] |> should equal "...-"

[<Fact>]
let ``morse to alpha lookup works correctly`` () =
    Alphabet.morseToAlpha.["...-"] |> should equal 'v'


type ``Given the sequence aaabb cc``() =
    let input = "aaabb cc"

    [<Fact>]
    member it.``when getting many a``() =
        let expected = [ 'a'; 'a'; 'a' ]

        let struct (result, rest) =
            Option.get (Parser.many (Parser.satisfy (fun c -> c = 'a')) input)

        result
        |> Seq.zip expected
        |> Seq.iter (fun (expectedItem, actualItem) -> actualItem |> should equal expectedItem)

        rest |> should equal ("bb cc")
