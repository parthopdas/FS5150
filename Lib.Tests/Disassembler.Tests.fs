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

let ``pargument tests data`` : obj array seq = 
    seq { 
        yield ("eAX", [||], ArgRegister AX)
        yield ("eCX", [||], ArgRegister CX)
        yield ("eBX", [||], ArgRegister BX)
        yield ("eDX", [||], ArgRegister DX)
        yield ("eSP", [||], ArgRegister SP)
        yield ("eBP", [||], ArgRegister BP)
        yield ("eSI", [||], ArgRegister SI)
        yield ("eDI", [||], ArgRegister DI)
        yield ("AX", [||], ArgRegister AX)
        yield ("BX", [||], ArgRegister BX)
        yield ("CX", [||], ArgRegister CX)
        yield ("DX", [||], ArgRegister DX)
        yield ("SP", [||], ArgRegister SP)
        yield ("BP", [||], ArgRegister BP)
        yield ("SI", [||], ArgRegister SI)
        yield ("DI", [||], ArgRegister DI)
        yield ("CS", [||], ArgRegister CS)
        yield ("DS", [||], ArgRegister DS)
        yield ("ES", [||], ArgRegister ES)
        yield ("SS", [||], ArgRegister SS)
        yield ("AL", [||], ArgRegister AL)
        yield ("BL", [||], ArgRegister BL)
        yield ("CL", [||], ArgRegister CL)
        yield ("DL", [||], ArgRegister DL)
        yield ("AH", [||], ArgRegister AH)
        yield ("BH", [||], ArgRegister BH)
        yield ("CH", [||], ArgRegister CH)
        yield ("DH", [||], ArgRegister DH)
        yield ("Eb", [| 0x00uy |], 
               ArgDereference { DrefType = MrmTBXSI
                                DrefDisp = None })
        yield ("Eb", [| 0x82uy; 0xDDuy; 0x7Euy |], 
               ArgDereference { DrefType = MrmTBPSI
                                DrefDisp = Some(W16 0x7eddus) })
        yield ("Eb", [| 0xC6uy |], ArgRegister DH)
        yield ("Ev", [| 0x42uy; 0xDDuy |], 
               ArgDereference { DrefType = MrmTBPSI
                                DrefDisp = Some(W8 0xdduy) })
        yield ("Ev", [| 0x06uy; 0xADuy; 0xBAuy |], 
               ArgDereference { DrefType = MrmTDisp
                                DrefDisp = Some(W16 0xbaadus) })
        yield ("Ev", [| 0xC4uy |], ArgRegister SP)
        yield ("Ew", [| 0xC7uy |], ArgRegister DI)
        yield ("Gb", [| 0x00uy |], ArgRegister AL)
        yield ("Gv", [| 0xD8uy |], ArgRegister BX)
        yield ("Jb", [| 0xEEuy |], ArgOffset(W8 0xeeuy))
        yield ("Jv", [| 0x0Duy; 0xF0uy |], ArgOffset(W16 0xf00dus))
        yield ("Ib", [| 0xF0uy |], ArgImmediate(W8 0xf0uy))
        yield ("Iv", [| 0xEFuy; 0xBEuy |], ArgImmediate(W16 0xbeefus))
        yield ("Iw", [| 0xADuy; 0xDEuy |], ArgImmediate(W16 0xdeadus))
        yield ("I0", [| 0x42uy |], ArgImmediate(W8 0x42uy))
        yield ("Ob", [| 0x0Duy; 0xF0uy |], 
               ArgDereference { DrefType = MrmTDisp
                                DrefDisp = Some(W16 0xf00dus) })
        yield ("Ob", [| 0xADuy; 0xDEuy |], 
               ArgDereference { DrefType = MrmTDisp
                                DrefDisp = Some(W16 0xdeadus) })
        yield ("Sw", [| 0x00uy |], ArgRegister ES)
        yield ("Sw", [| 0x08uy |], ArgRegister CS)
        yield ("Sw", [| 0x10uy |], ArgRegister SS)
        yield ("Sw", [| 0x18uy |], ArgRegister DS)
        yield ("Ap", [| 0x0Duy; 0xF0uy; 0xADuy; 0xDEuy |], 
               ArgAddress({ Segment = 0xdeadus
                            Offset = 0xf00dus }))
        yield ("Mp", [| 0x2Euy; 0xF0uy; 0xDDuy |], 
               ArgDereference { DrefType = MrmTDisp
                                DrefDisp = Some(W16 0xDDF0us) })
        yield ("Mp", [| 0xD8uy |], ArgRegister AX)
        yield ("1", [||], ArgConstant 1uy)
        yield ("3", [||], ArgConstant 3uy)
    }
    |> Seq.map (fun (a, b, c) -> 
           [| box a
              box b
              box c |])

[<Theory>]
[<MemberData("pargument tests data")>]
let ``pargument tests`` (desc, bs, res) : unit = 
    match runOnInput (pargument desc) (bs |> fromBytes) with
    | Success(arg, is) -> 
        arg |> should equal res
        is.Position.Offset |> should equal bs.Length
    | _ -> failwithf "Test failed: %A %A %A" desc bs res
