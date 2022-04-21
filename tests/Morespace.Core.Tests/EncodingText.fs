module Morespace.Core.Tests.EncodingText

open System
open Xunit
open FsUnit.Xunit

open Morespace.Core.MorseCode

type ``Given Alphabetic Text`` ()=
    let text = "hello world"

    [<Fact>]
    member it.``when parsing a single word token``() =
        let struct (result, rest) = Option.get (Morse.word text)
        (List.ofSeq result) |> should equal ['h';'e';'l';'l';'o']
        rest |> should equal "world"

    [<Fact>]
    member it.``when converting the text to Morse Code``() =
        let result = Option.get (Morse.encode text)
        result |> should equal ".... . .-.. .-.. ---\t.-- --- .-. .-.. -.."