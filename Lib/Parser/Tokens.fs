namespace Lib.Parser

module Tokens = 
    open Combinators
    open Core
    open System
    
    let pchar c = satisfy (c |> byte |> (=)) (c.ToString()) |>> char
    
    let pstring str = 
        str
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |>> (fun cs -> new String(cs |> Array.ofList))
    
    let pwhitespaceChar<'a> : Parser<char, 'a> = satisfy (char >> Char.IsWhiteSpace) "whitespace" |>> char
    let pwhitespace<'a> : Parser<string, 'a> = 
        pwhitespaceChar
        |> many
        |>> (fun cs -> new String(cs |> Array.ofList))
    let pdigit<'a> : Parser<char, 'a> = satisfy (char >> Char.IsDigit) "digit" |>> char
    let pinteger<'a> : Parser<int, 'a> = 
        pdigit
        |> many1
        |>> (fun cs -> new String(cs |> Array.ofList) |> int)
        <?> "integer"
    
    let psinteger<'a> : Parser<int, 'a> = 
        opt (pchar '-') .>>. pinteger |>> (fun (optSign, int) -> 
        match optSign with
        | None -> int
        | Some _ -> -int)
    
    let anyOf listOfChars = 
        listOfChars
        |> List.map pchar
        |> List.reduce (<|>)
    
    let plowerCase<'a> : Parser<char, 'a> = anyOf [ 'a'..'z' ] <?> "lowercase"
