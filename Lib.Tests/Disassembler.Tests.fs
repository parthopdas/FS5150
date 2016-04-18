module Lib.Disassembler.Tests

open Xunit
open FsCheck
open Lib.Disassembler
open Lib.Parser.Core
open Lib.Parser.TextInput
open System

[<Fact>]
let ``pword8 can parse word8``() = 
    let law b = 
        let x = runOnInput pword8 (fromBytes [| b |])
        x = Success(b, 
                    { Bytes = [| b |]
                      Position = { Offset = 1 } })
    Check.QuickThrowOnFailure law

[<Fact>]
let ``pword16 can parse word16``() = 
    let law n = 
        let num = (n + 1) * 9876 |> uint16
        let bytes = num |> BitConverter.GetBytes
        let res = runOnInput pword16 (bytes |> fromBytes)
        res = Success(num, 
                      { Bytes = bytes
                        Position = { Offset = 2 } })
    Check.QuickThrowOnFailure law

[<Fact>]
let ``pword32 can parse word32``() = 
    let law n = 
        let num = (n + 1) * 987654 |> uint32
        let bytes = num |> BitConverter.GetBytes
        let res = runOnInput pword32 (bytes |> fromBytes)
        res = Success(num, 
                      { Bytes = bytes
                        Position = { Offset = 4 } })
    Check.QuickThrowOnFailure law
