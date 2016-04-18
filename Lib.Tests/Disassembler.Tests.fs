module Lib.Disassembler.Tests

open Xunit
open FsCheck
open FsUnit.Xunit
open Lib.Disassembler
open Lib.Domain
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

let ``pmodRegRm tests data`` : obj array seq = 
    seq { 
        yield ([| 0b11000001uy |], MregT0, RmaReg MregT1)
        yield ([| 0b11011010uy |], MregT3, RmaReg MregT2)
        yield ([| 0b00100000uy |], MregT4, 
               RmaDeref { DrefType = MrmTBXSI
                          DrefDisp = None })
        yield ([| 0b01101001uy; 0xdeuy |], MregT5, 
               RmaDeref { DrefType = MrmTBXDI
                          DrefDisp = Some(W8 0xdeuy) })
        yield ([| 0b10110010uy; 0xaduy; 0xbauy |], MregT6, 
               RmaDeref { DrefType = MrmTBPSI
                          DrefDisp = Some(W16 0xbaadus) })
        yield ([| 0b10111011uy; 0x0duy; 0xf0uy |], MregT7, 
               RmaDeref { DrefType = MrmTBPDI
                          DrefDisp = Some(W16 0xf00dus) })
        yield ([| 0b01110100uy; 0xbeuy |], MregT6, 
               RmaDeref { DrefType = MrmTSI
                          DrefDisp = Some(W8 0xbeuy) })
        yield ([| 0b00101101uy |], MregT5, 
               RmaDeref { DrefType = MrmTDI
                          DrefDisp = None })
        yield ([| 0b00100110uy; 0x0duy; 0xf0uy |], MregT4, 
               RmaDeref { DrefType = MrmTDisp
                          DrefDisp = Some(W16 0xf00dus) })
        yield ([| 0b01011111uy; 0xebuy |], MregT3, 
               RmaDeref { DrefType = MrmTBX
                          DrefDisp = Some(W8 0xebuy) })
        yield ([| 0b10010110uy; 0xefuy; 0xbeuy |], MregT2, 
               RmaDeref { DrefType = MrmTBP
                          DrefDisp = Some(W16 0xbeefus) })
    }
    |> Seq.map (fun (a, b, c) -> 
           [| box a
              box b
              box c |])

[<Theory>]
[<MemberData("pmodRegRm tests data")>]
let ``pmodRegRm tests`` (bs, reg, rm) : unit = 
    match runOnInput pmodRegRm (bs |> fromBytes) with
    | Success(mrm, is) -> 
        mrm |> should equal { ModReg = reg
                              ModRM = rm }
        is.Position.Offset |> should equal bs.Length
    | _ -> failwithf "Test failed: %A %A %A" bs reg rm
