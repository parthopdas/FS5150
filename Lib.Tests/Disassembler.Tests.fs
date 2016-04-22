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
        yield ("eAX", [||], ArgRegister AX, false)
        yield ("eCX", [||], ArgRegister CX, false)
        yield ("eBX", [||], ArgRegister BX, false)
        yield ("eDX", [||], ArgRegister DX, false)
        yield ("eSP", [||], ArgRegister SP, false)
        yield ("eBP", [||], ArgRegister BP, false)
        yield ("eSI", [||], ArgRegister SI, false)
        yield ("eDI", [||], ArgRegister DI, false)
        yield ("AX", [||], ArgRegister AX, false)
        yield ("BX", [||], ArgRegister BX, false)
        yield ("CX", [||], ArgRegister CX, false)
        yield ("DX", [||], ArgRegister DX, false)
        yield ("SP", [||], ArgRegister SP, false)
        yield ("BP", [||], ArgRegister BP, false)
        yield ("SI", [||], ArgRegister SI, false)
        yield ("DI", [||], ArgRegister DI, false)
        yield ("CS", [||], ArgRegister CS, false)
        yield ("DS", [||], ArgRegister DS, false)
        yield ("ES", [||], ArgRegister ES, false)
        yield ("SS", [||], ArgRegister SS, false)
        yield ("AL", [||], ArgRegister AL, false)
        yield ("BL", [||], ArgRegister BL, false)
        yield ("CL", [||], ArgRegister CL, false)
        yield ("DL", [||], ArgRegister DL, false)
        yield ("AH", [||], ArgRegister AH, false)
        yield ("BH", [||], ArgRegister BH, false)
        yield ("CH", [||], ArgRegister CH, false)
        yield ("DH", [||], ArgRegister DH, false)
        yield ("Eb", [| 0x00uy |], 
               ArgDereference { DrefType = MrmTBXSI
                                DrefDisp = None }, true)
        yield ("Eb", [| 0x82uy; 0xDDuy; 0x7Euy |], 
               ArgDereference { DrefType = MrmTBPSI
                                DrefDisp = Some(W16 0x7eddus) }, true)
        yield ("Eb", [| 0xC6uy |], ArgRegister DH, true)
        yield ("Ev", [| 0x42uy; 0xDDuy |], 
               ArgDereference { DrefType = MrmTBPSI
                                DrefDisp = Some(W8 0xdduy) }, true)
        yield ("Ev", [| 0x06uy; 0xADuy; 0xBAuy |], 
               ArgDereference { DrefType = MrmTDisp
                                DrefDisp = Some(W16 0xbaadus) }, true)
        yield ("Ev", [| 0xC4uy |], ArgRegister SP, true)
        yield ("Ew", [| 0xC7uy |], ArgRegister DI, true)
        yield ("Gb", [| 0x00uy |], ArgRegister AL, true)
        yield ("Gv", [| 0xD8uy |], ArgRegister BX, true)
        yield ("Jb", [| 0xEEuy |], ArgOffset(W8 0xeeuy), false)
        yield ("Jv", [| 0x0Duy; 0xF0uy |], ArgOffset(W16 0xf00dus), false)
        yield ("Ib", [| 0xF0uy |], ArgImmediate(W8 0xf0uy), false)
        yield ("Iv", [| 0xEFuy; 0xBEuy |], ArgImmediate(W16 0xbeefus), false)
        yield ("Iw", [| 0xADuy; 0xDEuy |], ArgImmediate(W16 0xdeadus), false)
        yield ("I0", [| 0x42uy |], ArgImmediate(W8 0x42uy), false)
        yield ("Ob", [| 0x0Duy; 0xF0uy |], 
               ArgDereference { DrefType = MrmTDisp
                                DrefDisp = Some(W16 0xf00dus) }, false)
        yield ("Ob", [| 0xADuy; 0xDEuy |], 
               ArgDereference { DrefType = MrmTDisp
                                DrefDisp = Some(W16 0xdeadus) }, false)
        yield ("Sw", [| 0x00uy |], ArgRegister ES, true)
        yield ("Sw", [| 0x08uy |], ArgRegister CS, true)
        yield ("Sw", [| 0x10uy |], ArgRegister SS, true)
        yield ("Sw", [| 0x18uy |], ArgRegister DS, true)
        yield ("Ap", [| 0x0Duy; 0xF0uy; 0xADuy; 0xDEuy |], 
               ArgAddress({ Segment = 0xdeadus
                            Offset = 0xf00dus }), false)
        yield ("1", [||], ArgConstant 1uy, false)
        yield ("3", [||], ArgConstant 3uy, false)
        yield ("Mp", [| 0x2Euy; 0xF0uy; 0xDDuy |], 
               ArgDereference { DrefType = MrmTDisp
                                DrefDisp = Some(W16 0xDDF0us) }, true)
        yield ("Mp", [| 0xD8uy |], ArgRegister AX, true)
    }
    |> Seq.map (fun (a, b, c, d) -> 
           [| box a
              box b
              box c
              box d |])

[<Theory>]
[<MemberData("pargument tests data")>]
let ``pargument tests`` (desc, bs, res, hasMrm) : unit = 
    match runOnInput (pargument desc ([], None)) (bs |> fromBytes) with
    | Success((arg, mrm), is) -> 
        arg = [ res ] |> should equal true
        is.Position.Offset |> should equal bs.Length
        if hasMrm then mrm |> should not' (equal None)
        else mrm |> should equal None
    | _ -> failwithf "Test failed: %A %A %A" desc bs res

let instrSet = 
    { OpCodes = 
          [ (0x00uy, [ "XX0" ])
            (0x02uy, [ "XX2"; "arg0"; "arg1" ])
            (0x03uy, [ "GRP0" ])
            (0x04uy, [ "GRP1" ])
            (0x05uy, [ "GRP2"; "arg0o"; "arg0o" ])
            (0x06uy, [ "GRP3"; "arg0o"; "arg0o" ]) ]
          |> Map.ofList
      OpCodeGroups = 
          [ ({ OcgName = "GRP0"
               OcgIndex = 5uy }, [ "EXX00" ])
            ({ OcgName = "GRP0"
               OcgIndex = 1uy }, [ "EXX01" ])
            ({ OcgName = "GRP1"
               OcgIndex = 0uy }, [ "EXX1"; "arg00"; "arg01" ])
            ({ OcgName = "GRP2"
               OcgIndex = 3uy }, [ "EXX2" ])
            ({ OcgName = "GRP2"
               OcgIndex = 4uy }, [ "EXX2"; "arg0x"; "arg0x" ])
            ({ OcgName = "GRP3"
               OcgIndex = 7uy }, [ "--" ]) ]
          |> Map.ofList }

let ``popCode tests data`` : obj array seq = 
    seq { 
        yield ((*0 arg*)[| 0x00uy |], "XX0", [], false)
        yield ((*2 arg*) [| 0x02uy |], "XX2", [ "arg0"; "arg1" ], false)
        yield ((*ex 0 arg*) [| 0x03uy; 0b00101000uy |], "EXX00", [], true)
        yield ((*ex 0 arg - different reg*) [| 0x03uy; 0b00001000uy |], "EXX01", [], true)
        yield ((*ex 2 arg*) [| 0x04uy; 0b00000000uy |], "EXX1", [ "arg00"; "arg01" ], true)
        yield ((*ex 0 2 arg from op*) [| 0x05uy; 0b00011000uy |], "EXX2", [ "arg0o"; "arg0o" ], true)
        yield ((*ex 2 overide arg*) [| 0x05uy; 0b00100000uy |], "EXX2", [ "arg0x"; "arg0x" ], true)
        yield ((*ex illegal*) [| 0x06uy; 0b00111000uy |], "???", [], true)
    }
    |> Seq.map (fun (a, b, c, d) -> 
           [| box a
              box b
              box c
              box d |])

[<Theory>]
[<MemberData("popCode tests data")>]
let ``popCode tests`` (bs, oc, args, hasMrm) : unit = 
    match runOnInput (popCode instrSet) (bs |> fromBytes) with
    | Success((o, a, m), is) -> 
        (o, a) |> should equal (oc, args)
        if hasMrm then m |> should not' (equal None)
        else m |> should equal None
        is.Position.Offset |> should equal bs.Length
    | Failure(pl, pe, pp) -> failwithf "Test failed: %A %A %A %A: %A %A %A" bs oc args hasMrm pl pe pp

let ``pinstruction tests data`` : obj array seq = 
    seq { 
        (* 4 Args *)        yield ([| 0x11uy; 0b00101110uy; 0xF0uy; 0xDDuy |], "0000:0000 ADC\t [DDF0], BP") 
        (* 6 Args + GRP *)  yield ([| 0x81uy; 0x06uy; 0x34uy; 0x01uy; 0x32uy; 0x00uy |], "0000:0000 ADD\t [0134], 0032") 
    } 
    |> Seq.map (fun (a, b) -> 
        [| box a
           box b |])

[<Theory>]
[<MemberData("pinstruction tests data")>]
let ``pinstruction tests`` (bs, instr) : unit = 
    let is = 
        Lib.InstructionSetLoader.loadInstructionSet 
            (System.IO.File.ReadAllText(@"C:\src\bb\5150\pyd4sm\8086_table.txt"))
    match runOnInput (pinstruction (0us, 0us) is) (bs |> fromBytes) with
    | Success(i, is) -> 
        i.ToString() |> should equal instr
        is.Position.Offset |> should equal bs.Length
    | Failure(pl, pe, pp) -> failwithf "Test failed: %A %A: %A %A %A" bs instr pl pe pp

// test for address in pinstruction tests
// move file to project
// 1 2 3 4 5 6 + GRP tests
