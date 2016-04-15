namespace Lib.Parser

module Tokens = 
    open Combinators
    open Core
    open System
    
    let pbyte c = satisfy (fun c' -> c' = c) (c.ToString())

    let pchar c = satisfy (fun c' -> c' = (c |> byte)) (c.ToString()) |>> char
    
    let pstring str = 
        str
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |>> (fun cs -> new String(cs |> Array.ofList))
    
    let pwhitespaceChar = satisfy (char >> Char.IsWhiteSpace) "whitespace" |>> char
    let pwhitespace = pwhitespaceChar
                      |> many
                      |>> (fun cs -> new String(cs |> Array.ofList))
    let pdigit = satisfy (char >> Char.IsDigit) "digit" |>> char
    let pinteger = pdigit
                   |> many1
                   |>> (fun cs -> new String(cs |> Array.ofList) |> int)
                   <?> "integer"
    
    let psinteger = 
        opt (pchar '-') .>>. pinteger |>> (fun (optSign, int) -> 
        match optSign with
        | None -> int
        | Some _ -> -int)
    
    let anyOf listOfChars = 
        listOfChars
        |> List.map pchar
        |> List.reduce (<|>)
    
    let plowerCase = anyOf [ 'a'..'z' ] <?> "lowercase"
