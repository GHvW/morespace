module Morespace.Core.MorseCode.Morse

open System
open Morespace.Core


let text: Parser.Parser<string> =
    Parser.token (Parser.alphaNumeric |> Parser.map String.Concat)


let encode text = ""
