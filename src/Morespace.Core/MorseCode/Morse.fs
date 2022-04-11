module Morespace.Core.MorseCode.Morse

open System
open Morespace.Core.Parser // full module
open Morespace.Core.MorseCode.Alphabet // full module


let word: Parser<seq<char>> =
    token (atLeastOne alphaNumeric)


let charToMorse (c: char) : Option<string> =
    let mutable morseRepresentation = "" 
    if (alphaToMorse.TryGetValue(c, &morseRepresentation)) then
        Some morseRepresentation
    else
        None



let encode text = 
    atLeastOne (word |> map (Seq.map charToMorse)) |> map (Seq)