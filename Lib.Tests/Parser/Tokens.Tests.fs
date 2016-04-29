module Lib.Parser.Tokens.Tests

open Xunit
open FsCheck
open FsUnit.Xunit
open Lib.Parser.Core
open Lib.Parser.Tokens
open System

[<Theory>]
[<InlineData("aBC", "'a' [State: 1 aBC]")>]
let ``parseLowerCase with good input`` (i, r) = 
    let res = i |> run plowerCase ()
    res
    |> printResult
    |> should equal r

[<Theory>]
[<InlineData("ABC", "0: ABC: Error parsing lowercase. Unexpected 'A'")>]
let ``parseLowerCase with bad input`` (i, m) = 
    let res = i |> run plowerCase ()
    res
    |> printResult
    |> should equal m

[<Theory>]
[<InlineData("1ABC", "'1' [State: 1 1ABC]")>]
[<InlineData("9ABC", "'9' [State: 1 9ABC]")>]
let ``parseDigit with good input`` (i, r) = 
    let res = i |> run pdigit ()
    res
    |> printResult
    |> should equal r

[<Theory>]
[<InlineData("|ABC", "0: |ABC: Error parsing digit. Unexpected '|'")>]
let ``parseDigit with bad input`` (i, m) = 
    let res = i |> run pdigit ()
    res
    |> printResult
    |> should equal m
