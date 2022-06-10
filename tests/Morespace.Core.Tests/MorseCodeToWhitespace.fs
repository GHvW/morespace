module Morespace.Core.Tests.MorseCodeToWhitespace

open System
open Xunit
open FsUnit.Xunit

open Morespace.Core
open Morespace.Core.WhitespaceMorse

// [<Fact>]
// let ``My test`` () = Assert.True(true)


type ``Given a sequence of whitespace characters``() =

    let morseHello = ".... . .-.. .-.. ---"
    let morseWorld = ".-- --- .-. .-.. -.."
    let morseHelloWorld = morseHello + "\t" + morseWorld

    let hello =
        " \t \t \t "
        + "\t\t\t"
        + " "
        + "\t\t\t"
        + " \t   \t \t "
        + "\t\t\t"
        + " \t   \t \t "
        + "\t\t\t"
        + "   \t   \t   "

    let world =
        " \t   \t   "
        + "\t\t\t"
        + "   \t   \t   "
        + "\t\t\t"
        + " \t   \t "
        + "\t\t\t"
        + " \t   \t \t "
        + "\t\t\t"
        + "   \t \t "

    let phrase = hello + "\t\t\t\t\t\t\t" + world

    [<Fact>]
    member it.``when parsing a whitespace word``() =
        let result =
            Whitespace.morseWordToWhitespace morseHello

        result |> should equal hello


    [<Fact>]
    member it.``when parsing a whitespace phrase``() =
        let result =
            Whitespace.morseToWhitespace morseHelloWorld

        result |> should equal phrase
