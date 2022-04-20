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


let traverse (it: seq<Option<'A>>) : Option<list<'A>> =
    let rec inner result rest =
        if Seq.isEmpty rest then
            Some result
        else
            match (Seq.head rest) with 
            | None -> None
            | Some item ->
                inner (item::result) (Seq.tail rest)
                
    inner [] it


let private charsToMorse: seq<char> -> Option<list<string>> = 
    (Seq.map charToMorse) >> traverse


// TODO - spacing
let private convertToMorse: seq<seq<char>> -> Option<string> = 
    (Seq.map charsToMorse) 
    >> traverse 
    >> (Option.map (List.concat >> (String.concat " ")))


let encode text =
    text
    |> (atLeastOne word |> map convertToMorse)
    |> Option.bind (fun struct (encodedData, _) ->
        encodedData)
    