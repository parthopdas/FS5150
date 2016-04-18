﻿module Lib.Parser.Core.Tests

open FsUnit.Xunit
open Lib.Parser.Combinators
open Lib.Parser.Tokens
open Xunit

[<Theory>]
[<InlineData('A', "ABC", "'A' [State: (0, 1) ABC]")>]
[<InlineData('Z', "ZBC", "'Z' [State: (0, 1) ZBC]")>]
let ``Parser with good input`` (c, i, r) = 
    let parseChar = pchar c
    let res = i |> run parseChar
    res
    |> printResult
    |> should equal r

[<Theory>]
[<InlineData('X', "", "(0, 0): : Error parsing X. No more input")>]
[<InlineData('A', "ZBC", "(0, 0): ZBC: Error parsing A. Unexpected 'Z'")>]
let ``Parser with bad input`` (c, i, s) = 
    let parseChar = pchar c
    let res = i |> run parseChar
    res
    |> printResult
    |> should equal s

let parseA = pchar 'A'
let parseB = pchar 'B'

[<Theory>]
[<InlineData("ABC", "('A', 'B') [State: (0, 2) ABC]")>]
let ``andThen with good input`` (i, r) = 
    let parseAandThenB = parseA .>>. parseB
    let res = i |> run parseAandThenB
    res
    |> printResult
    |> should equal r

[<Theory>]
[<InlineData("ZBC", "(0, 0): ZBC: Error parsing A. Unexpected 'Z'")>]
[<InlineData("AZC", "(0, 1): AZC: Error parsing B. Unexpected 'Z'")>]
let ``andThen with bad input`` (i, m) = 
    let parseAandThenB = parseA .>>. parseB
    let res = i |> run parseAandThenB
    res
    |> printResult
    |> should equal m

[<Theory>]
[<InlineData("AZZ", "'A' [State: (0, 1) AZZ]")>]
[<InlineData("BZZ", "'B' [State: (0, 1) BZZ]")>]
let ``orElse with good input`` (i, r) = 
    let parseAorElseB = parseA <|> parseB
    let res = i |> run parseAorElseB
    res
    |> printResult
    |> should equal r

[<Theory>]
[<InlineData("CZZ", "(0, 0): CZZ: Error parsing B. Unexpected 'C'")>]
let ``orElse with bad input`` (i, m) = 
    let parseAorElseB = parseA <|> parseB
    let res = i |> run parseAorElseB
    res
    |> printResult
    |> should equal m

let parseC = pchar 'C'
let parseAandThenBOrC = parseA .>>. (parseB <|> parseC)

[<Theory>]
[<InlineData("ABZ", "('A', 'B') [State: (0, 2) ABZ]")>]
[<InlineData("ACZ", "('A', 'C') [State: (0, 2) ACZ]")>]
let ``andThenOrElse with good input`` (i, r) = 
    let res = i |> run parseAandThenBOrC
    res
    |> printResult
    |> should equal r

[<Theory>]
[<InlineData("QBZ", "(0, 0): QBZ: Error parsing A. Unexpected 'Q'")>]
[<InlineData("AQZ", "(0, 1): AQZ: Error parsing C. Unexpected 'Q'")>]
let ``andThenOrElse with bad input`` (i, m) = 
    let res = i |> run parseAandThenBOrC
    res
    |> printResult
    |> should equal m