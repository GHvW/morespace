module Morespace.Core.WhitespaceMorse.Whitespace

// space for dot
// 3 space for dash
// tab between characters
// 3 tab between letters
// 7 tab for word
open Morespace.Core.MorseCode
open Morespace.Core

let morseCharToWhitespace (morseChar: char) : string =
    match morseChar with
    | '.' -> " "
    | '-' -> "   " // 3 spaces
    | ' ' -> "\t\t\t"
    | '\t' -> "\t\t\t\t\t\t\t" // 7 tabs
    | _ -> " ERROR " // TODO - refactor to DU to get rid of this?



let morseWordToWhitespace (morseWord: string) : string =
    morseWord.Split(' ')
    |> Seq.map (
        Seq.map morseCharToWhitespace
        >> String.concat "\t"
    )
    |> String.concat "\t\t\t"



let morseToWhitespace (morsePhrase: string) =
    morsePhrase.Split('\t')
    |> Seq.map (morseWordToWhitespace)
    |> String.concat "\t\t\t\t\t\t\t"
