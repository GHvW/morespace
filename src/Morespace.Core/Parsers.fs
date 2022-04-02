module Morespace.Core.Parsers

type Parser<'A> = string -> Option<struct ('A * string)>

let zero: Parser<'A> = fun input -> None

let success (item: 'A) : Parser<'A> = fun input -> Some(item, input)

let orElse (first: Parser<'A>) (other: Parser<'A>) : Parser<'A> =
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


let andAlso (first: Parser<'A>) (other: Parser<'B>) : Parser<struct ('A * 'B)> =
    parser {
        let! a = first
        let! b = other
        return (a, b)
    }


