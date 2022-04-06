module Morespace.Core.Parser

open System
open System.Collections.Generic
open Morespace.Core.MorseCode.Alphabet

// Overkill? Maybe. Fun? Absolutely!

type Parser<'A> = string -> Option<struct ('A * string)>

let zero: Parser<'A> = fun _ -> None

let success (item: 'A) : Parser<'A> = fun input -> Some(item, input)

let orElse (other: Parser<'A>) (first: Parser<'A>) : Parser<'A> =
    fun input -> first input |> Option.orElse (other input)

let map (fn: 'A -> 'B) (parser: Parser<'A>) : Parser<'B> =
    fun input ->
        parser input
        |> Option.map (fun struct (item, rest) -> (fn item, rest))


let bind (fn: 'A -> Parser<'B>) (parser: Parser<'A>) : Parser<'B> =
    fun input ->
        parser input
        |> Option.bind (fun struct (item, rest) -> fn item rest)

let item : Parser<char> =
    fun input ->
        if String.length input = 0 then
            None
        else
            Some (input[0], input[1..])


let satisfy (predicate: char -> bool) : Parser<char> =
    bind (fun it ->
        if predicate it then
            success it
        else
            zero)
        item

type ParserBuilder() =
    member this.Zero() = zero
    member this.Bind(parser, func) = bind func parser
    member this.Return(item) = success item

let parser = ParserBuilder()


let andAlso (other: Parser<'B>) (first: Parser<'A>) : Parser<struct ('A * 'B)> =
    parser {
        let! a = first
        let! b = other
        return (a, b)
    }


let rec many (parse: Parser<'A>) : Parser<seq<'A>> =
    parser {
        let! item = parse
        let! rest = many parse
        return seq {
            yield item
            yield! rest
        }
    } |> orElse (success [])


let atLeastOne (parse: Parser<'A>) : Parser<seq<'A>> =
    parser {
        let! item = parse
        let! rest = many parse
        return seq {
            yield item
            yield! rest
        }
    }

let character (c: char) : Parser<char> =
    satisfy (fun it -> it = c)

let morse : Parser<char> =
    satisfy (fun c -> c = '.' || c = '-')


let whitespace : Parser<char> =
    satisfy Char.IsWhiteSpace


let space : Parser<char> =
    satisfy (fun it -> it <> ' ' && Char.IsWhiteSpace(it))


let token (parse: Parser<'A>) : Parser<'A> =
    parser {
        let! item = parse
        let! _ = many whitespace
        return item
    }

let atLeastOneSeparatedBy (separator: Parser<'B>) (parse: Parser<'A>) : Parser<seq<'A>> =
    parser {
        let! item = parse
        let! rest = many (separator |> bind (fun _ -> parse))
        return seq {
            yield item
            yield! rest
        }
    }

let convertMorseToAlpha (morseChar: string) : Option<char> =
    let mutable alpha = ' '
    if (morseToAlpha.TryGetValue (morseChar, &alpha)) then
        Some alpha
    else
        None

let morseCharacter : Parser<char> =
    morse
    |> many
    |> bind (fun chars ->
        printfn "%A" chars
        let alphaVal = chars |> String.Concat |> convertMorseToAlpha 
        printfn "%A" alphaVal
        match alphaVal with
        | None -> zero
        | Some a -> success a)
    

let word : Parser<string> =
    token (morseCharacter 
           |> atLeastOneSeparatedBy (character ' ') 
           |> map String.Concat)


let morseCode : Parser<string> =
    atLeastOne word |> map (String.concat " ")


let whiteSpaceMorse : Parser<char> =
    character '\t' |> map (fun _ -> '-')
    |> orElse (character ' ' |> map (fun _ -> '.'))

