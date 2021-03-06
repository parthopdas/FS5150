﻿module Lib.Chips.I8088.Disassembler.Tests

open Xunit
open FsCheck
open FsUnit.Xunit
open Lib.Chips.I8088.InstructionSet
open Lib.Parser.Core
open Lib.Parser.TextInput
open System
open Lib.Chips.I8088.Disassembler
open Lib.Common
open Lib.Chips.I8088.InstructionSetLoader
open YaFunTK.Result

[<Fact>]
let ``pword8 can parse word8``() = 
    let law b = 
        let x = runOnInput pword8 (fromBytes () [| b |])
        x = ParserResult.Success(b, 
                    { Bytes = [| b |]
                      Position = 1
                      UserState = () })
    Check.QuickThrowOnFailure law

[<Fact>]
let ``pword16 can parse word16``() = 
    let law n = 
        let num = (n + 1) * 9876 |> uint16
        let bytes = num |> BitConverter.GetBytes
        let res = runOnInput pword16 (bytes |> fromBytes ())
        res = ParserResult.Success(num, 
                      { Bytes = bytes
                        Position = 2
                        UserState = () })
    Check.QuickThrowOnFailure law

[<Fact>]
let ``pword32 can parse word32``() = 
    let law n = 
        let num = (n + 1) * 987654 |> uint32
        let bytes = num |> BitConverter.GetBytes
        let res = runOnInput pword32 (bytes |> fromBytes ())
        res = ParserResult.Success(num, 
                      { Bytes = bytes
                        Position = 4
                        UserState = () })
    Check.QuickThrowOnFailure law

let ``pmodRegRm tests data`` : obj array seq = 
    seq { 
        yield ([| 0b11000001uy |], 0, RmaReg 1uy, false)
        yield ([| 0b11011010uy |], 3, RmaReg 2uy, false)
        yield ([| 0b00100000uy |], 4,  
               RmaDeref (MrmTBXSI, None), false)
        yield ([| 0b01101001uy; 0xdeuy |], 5,  
               RmaDeref (MrmTBXDI, Some(W8 0xdeuy)), false)
        yield ([| 0b10110010uy; 0xaduy; 0xbauy |], 6, 
               RmaDeref (MrmTBPSI, Some(W16 0xbaadus)), true)
        yield ([| 0b10111011uy; 0x0duy; 0xf0uy |], 7, 
               RmaDeref (MrmTBPDI, Some(W16 0xf00dus)), true)
        yield ([| 0b01110100uy; 0xbeuy |], 6,
               RmaDeref (MrmTSI, Some(W8 0xbeuy)), false)
        yield ([| 0b00101101uy |], 5, 
               RmaDeref (MrmTDI, None), false)
        yield ([| 0b00100110uy; 0x0duy; 0xf0uy |], 4, 
               RmaDeref (MrmTDisp, Some(W16 0xf00dus)), false)
        yield ([| 0b01011111uy; 0xebuy |], 3, 
               RmaDeref (MrmTBX, Some(W8 0xebuy)), false)
        yield ([| 0b10010110uy; 0xefuy; 0xbeuy |], 2, 
               RmaDeref (MrmTBP, Some(W16 0xbeefus)), true)
    }
    |> Seq.map (fun (a, b, c, d) -> 
           [| box a
              box b
              box c
              box d |])

[<Theory>]
[<MemberData("pmodRegRm tests data")>]
let ``pmodRegRm tests`` (bs, reg, rm, usess) : unit = 
    match runOnInput pmodRegRm (bs |> fromBytes ()) with
    | ParserResult.Success(mrm, is) -> 
        mrm |> should equal { ModRM = rm
                              MRReg = reg
                              MRUseSS = usess }
        is.Position |> should equal bs.Length
    | _ -> failwithf "Test failed: %A %A %A" bs reg rm

let ``pargument tests data`` : obj array seq = 
    seq { 
        yield ("eAX", [||], ArgRegister16 AX, false)
        yield ("eCX", [||], ArgRegister16 CX, false)
        yield ("eBX", [||], ArgRegister16 BX, false)
        yield ("eDX", [||], ArgRegister16 DX, false)
        yield ("eSP", [||], ArgRegister16 SP, false)
        yield ("eBP", [||], ArgRegister16 BP, false)
        yield ("eSI", [||], ArgRegister16 SI, false)
        yield ("eDI", [||], ArgRegister16 DI, false)
        yield ("AX", [||], ArgRegister16 AX, false)
        yield ("BX", [||], ArgRegister16 BX, false)
        yield ("CX", [||], ArgRegister16 CX, false)
        yield ("DX", [||], ArgRegister16 DX, false)
        yield ("SP", [||], ArgRegister16 SP, false)
        yield ("BP", [||], ArgRegister16 BP, false)
        yield ("SI", [||], ArgRegister16 SI, false)
        yield ("DI", [||], ArgRegister16 DI, false)
        yield ("CS", [||], ArgRegisterSeg CS, false)
        yield ("DS", [||], ArgRegisterSeg DS, false)
        yield ("ES", [||], ArgRegisterSeg ES, false)
        yield ("SS", [||], ArgRegisterSeg SS, false)
        yield ("AL", [||], ArgRegister8 AL, false)
        yield ("BL", [||], ArgRegister8 BL, false)
        yield ("CL", [||], ArgRegister8 CL, false)
        yield ("DL", [||], ArgRegister8 DL, false)
        yield ("AH", [||], ArgRegister8 AH, false)
        yield ("BH", [||], ArgRegister8 BH, false)
        yield ("CH", [||], ArgRegister8 CH, false)
        yield ("DH", [||], ArgRegister8 DH, false)
        yield ("Eb", [| 0x00uy |], 
               ArgDereference8 { DrefType8 = MrmTBXSI
                                 DrefDisp8 = None }, true)
        yield ("Eb", [| 0x82uy; 0xDDuy; 0x7Euy |], 
               ArgDereference8 { DrefType8 = MrmTBPSI
                                 DrefDisp8 = Some(W16 0x7eddus) }, true)
        yield ("Eb", [| 0xC6uy |], ArgRegister8 DH, true)
        yield ("Ev", [| 0x42uy; 0xDDuy |], 
               ArgDereference16 { DrefType16 = MrmTBPSI
                                  DrefDisp16 = Some(W8 0xdduy) }, true)
        yield ("Ev", [| 0x06uy; 0xADuy; 0xBAuy |], 
               ArgDereference16 { DrefType16 = MrmTDisp
                                  DrefDisp16 = Some(W16 0xbaadus) }, true)
        yield ("Ev", [| 0xC4uy |], ArgRegister16 SP, true)
        yield ("Ew", [| 0xC7uy |], ArgRegister16 DI, true)
        yield ("Gb", [| 0x00uy |], ArgRegister8 AL, true)
        yield ("Gv", [| 0xD8uy |], ArgRegister16 BX, true)
        yield ("Jb", [| 0xEEuy |], ArgOffset(0xffeeus), false)
        yield ("Jv", [| 0x0Duy; 0xF0uy |], ArgOffset(0xf00dus), false)
        yield ("Ib", [| 0xF0uy |], ArgImmediate(W8 0xf0uy), false)
        yield ("Iv", [| 0xEFuy; 0xBEuy |], ArgImmediate(W16 0xbeefus), false)
        yield ("Iw", [| 0xADuy; 0xDEuy |], ArgImmediate(W16 0xdeadus), false)
        yield ("I0", [| 0x42uy |], ArgImmediate(W8 0x42uy), false)
        yield ("Ob", [| 0x0Duy; 0xF0uy |], 
               ArgDereference8 { DrefType8 = MrmTDisp
                                 DrefDisp8 = Some(W16 0xf00dus) }, false)
        yield ("Ob", [| 0xADuy; 0xDEuy |], 
               ArgDereference8 { DrefType8 = MrmTDisp
                                 DrefDisp8 = Some(W16 0xdeadus) }, false)
        yield ("Sw", [| 0x00uy |], ArgRegisterSeg ES, true)
        yield ("Sw", [| 0x08uy |], ArgRegisterSeg CS, true)
        yield ("Sw", [| 0x10uy |], ArgRegisterSeg SS, true)
        yield ("Sw", [| 0x18uy |], ArgRegisterSeg DS, true)
        yield ("Ap", [| 0x0Duy; 0xF0uy; 0xADuy; 0xDEuy |], 
               ArgAddress({ Segment = 0xdeadus
                            Offset = 0xf00dus }), false)
        yield ("1", [||], ArgConstant 1uy, false)
        yield ("3", [||], ArgConstant 3uy, false)
        yield ("Mp", [| 0x2Euy; 0xF0uy; 0xDDuy |], 
               ArgDereference16 { DrefType16 = MrmTDisp
                                  DrefDisp16 = Some(W16 0xDDF0us) }, true)
        yield ("Mp", [| 0xD8uy |], ArgRegister16 AX, true)
    }
    |> Seq.map (fun (a, b, c, d) -> 
           [| box a
              box b
              box c
              box d |])

[<Theory>]
[<MemberData("pargument tests data")>]
let ``pargument tests`` (desc, bs, res, hasMrm) : unit = 
    match runOnInput (pargument (toOcArg desc) ([], None)) (bs |> fromBytes ()) with
    | ParserResult.Success((arg, mrm), is) -> 
        arg = [ res ] |> should equal true
        is.Position |> should equal bs.Length
        if hasMrm then mrm |> should not' (equal None)
        else mrm |> should equal None
    | _ -> failwithf "Test failed: %A %A %A" desc bs res

let instrSet = 
    { OpCodes = 
          [ (0x00uy, { OcName = "XX0"; OcArgs = [||]})
            (0x02uy, { OcName = "XX2"; OcArgs = [| "arg0"; "arg1" |]})
            (0x03uy, { OcName = "GRP0"; OcArgs = [||]})
            (0x04uy, { OcName = "GRP1"; OcArgs = [||]})
            (0x05uy, { OcName = "GRP2"; OcArgs = [| "arg0o"; "arg0o" |]})
            (0x06uy, { OcName = "GRP3"; OcArgs = [| "arg0o"; "arg0o" |]}) ]
          |> Map.ofList
      OpCodeGroups = 
          [ ({ OcgName = "GRP0"
               OcgIndex = 5uy }, { OcName = "EXX00"; OcArgs = [||]})
            ({ OcgName = "GRP0"
               OcgIndex = 1uy }, { OcName = "EXX01"; OcArgs = [||]})
            ({ OcgName = "GRP1"
               OcgIndex = 0uy }, { OcName = "EXX1"; OcArgs = [|"arg00"; "arg01"|]})
            ({ OcgName = "GRP2"
               OcgIndex = 3uy }, { OcName = "EXX2"; OcArgs = [||]})
            ({ OcgName = "GRP2"
               OcgIndex = 4uy }, { OcName = "EXX2"; OcArgs = [|"arg0x"; "arg0x"|]})
            ({ OcgName = "GRP3"
               OcgIndex = 7uy }, { OcName = "--"; OcArgs = [||]}) ]
          |> Map.ofList }

let ``popCode tests data`` : obj array seq = 
    seq { 
        yield ([| (*0 arg*) 0x27uy |], "DAA", [||], false)
        yield ([| (*2 arg*) 0x20uy |], "AND", [| OcaNormal(NormalArgCode.E ||| NormalArgCode.B); OcaNormal(NormalArgCode.G ||| NormalArgCode.B) |], false)
        yield ([| (*ex 2 override arg*) 0xF7uy; 0b00000000uy |], "TEST", [| OcaNormal(NormalArgCode.E ||| NormalArgCode.W); OcaNormal(NormalArgCode.I ||| NormalArgCode.W) |], true)
        yield ([| (*ex 0 2 arg from op*) 0xD3uy; 0b00000000uy |], "ROL", [| OcaNormal(NormalArgCode.E ||| NormalArgCode.W); OcaSpecial(SpecialArgCode.CL) |], true)
        yield ([| (*ex 1 arg*) 0xFEuy; 0b00000000uy |], "INC", [| OcaNormal(NormalArgCode.E ||| NormalArgCode.B) |], true)
        yield ([| (*ex illegal*) 0xFFuy; 0b00111000uy |], "--", [| OcaNormal(NormalArgCode.E ||| NormalArgCode.W) |], true)
    }
    |> Seq.map (fun (a, b, c, d) -> 
           [| box a
              box b
              box c
              box d |])

[<Theory>]
[<MemberData("popCode tests data")>]
let ``popCode tests`` (bs, oc, args, hasMrm) : unit = 
    match runOnInput (popCode grammer) (bs |> fromBytes ()) with
    | ParserResult.Success((ocd, m), is) -> 
        (ocIndices.[ocd.OcId], ocd.OcArgs) |> should equal (oc, args)
        if hasMrm then m |> should not' (equal None)
        else m |> should equal None
        is.Position |> should equal bs.Length
    | ParserResult.Failure(pl, pe, pp) -> failwithf "Test failed: %A %A %A %A: %A %A %A" bs oc args hasMrm pl pe pp

let ``pinstruction tests data`` : obj array seq = 
    seq {    
        (* 1 Arg  *)        yield ([| 0x26uy; |], "0000:0000 26           ES:* ", false) 
        (* 1 Arg  *)        yield ([| 0x37uy; |], "0000:0000 37           AAA ", false) 
        (* 2 Arg  *)        yield ([| 0x04uy; 0xFFuy |], "0000:0000 04FF         ADD AL, FF", false) 
        (* 3 Arg  *)        yield ([| 0xE8uy; 0x0Duy; 0xF0uy |], "0000:0000 E80DF0       CALL F00D", false) 
        (* 4 Args *)        yield ([| 0x11uy; 0b00101110uy; 0xF0uy; 0xDDuy |], "0000:0000 112EF0DD     ADC word ptr [DDF0], BP", false) 
        (* 5 Args *)        yield ([| 0xEAuy; 0x0Duy; 0xF0uy; 0xADuy; 0xBAuy |], "0000:0000 EA0DF0ADBA   JMP BAAD:F00D", false) 
        (* 6 Args + GRP *)  yield ([| 0x81uy; 0b10000110uy; 0x34uy; 0x01uy; 0x32uy; 0x00uy |], "0000:0000 818634013200 ADD word ptr [BP+0134], 0032", true) 
        (* 6 Args *)        yield ([| 0xC7uy; 0x06uy; 0x72uy; 0x00uy; 0x00uy; 0x00uy |], "0000:0000 C70672000000 MOV word ptr [0072], 0000", false) 
    }
    |> Seq.map (fun (a, b, c) -> 
           [| box a
              box b
              box c |])

[<Theory>]
[<MemberData("pinstruction tests data")>]
let ``pinstruction tests`` (bs, instr, usess) : unit = 
    let csip = { Segment = 0us; Offset = 0us }
    match runOnInput (pinstruction csip grammer) (bs |> fromBytes 0) with
    | ParserResult.Success(i, is) -> 
        i.ToString() |> should equal instr
        i.UseSS |> should equal usess
        is.Position |> should equal bs.Length
    | ParserResult.Failure(pl, pe, pp) -> failwithf "Test failed: %A %A: %A %A %A" bs instr pl pe pp

[<Fact>]
let ``pinstruction parse-compile round trip``() = 
    // TODO: P2D: Implement fully when we are able to compile Instruction back to bytes
    // It can be done now that we are tracking the bytes as well.
    // As soon as we have figured out how to get a generator for multiple args, implement it.
    let law csip = 
        match runOnInput (pinstruction csip grammer) ([| 0x37uy |] |> fromBytes 0) with
        | ParserResult.Success(i, is) -> i.ToString() = (sprintf "%O 37           AAA " csip) && is.Position = 1
        | ParserResult.Failure _ -> false
    Check.QuickThrowOnFailure law
