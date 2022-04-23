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


// TODO - use a queue? or C# List ?
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
    (Seq.map (charsToMorse >> Option.map (List.rev >> String.concat " ")))
    >> traverse 
    >> (Option.map (List.rev >> (String.concat "\t")))


let encoder: Parser<Option<string>> =
    (atLeastOne word |> map convertToMorse)


let private structFst struct (data, _) = data
    

let encode = encoder >> (Option.bind structFst)