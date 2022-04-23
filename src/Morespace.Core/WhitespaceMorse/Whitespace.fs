module Morespace.Core.WhitespaceMorse.Whitespace

// space for dot
// 3 space for dash
// tab between characters
// 3 tab between letters
// 7 tab for word
open Morespace.Core.MorseCode
open Morespace.Core

let morseConvert morseChar =
    match morseChar with
    | '.' -> " "
    | '-' -> "   " // 3 spaces
    | ' ' -> "\t"
    | '\t' -> "\t\t\t\t\t\t\t" // 7 tabs
    | _ -> "This shouldn't happen" // TODO - refactor to DU to get rid of this?



// let morseToWhitespace =
//     encoder


// let whitespaceToMorse = 