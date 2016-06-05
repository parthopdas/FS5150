module Lib.InstructionSetLoader.Tests

open FsUnit.Xunit
open Lib.CPU.InstructionSetLoader
open Lib.Common
open Lib.Domain.InstructionSet
open Xunit

[<Fact>]
let ``Ignore comments and empty lines``() = 
    let lines = "\r\n*  asda\n"
    let is = loadInstructionSet lines
    { OpCodes = Map.empty
      OpCodeGroups = Map.empty }
    |> should equal is

let opCodetestData : seq<obj []> = 
    [ "DB\t--", 0xDBuy, 
      { OcName = "--"
        OcArgs = [||] }, (1, 0)
      "20\tAND \t\tEb  Gb", 0x20uy, 
      { OcName = "AND"
        OcArgs = [| "Eb"; "Gb" |] }, (1, 0) ]
    |> Seq.map (fun (a, b, c, d) -> [| a; b; c; d |])

[<Theory>]
[<MemberData("opCodetestData")>]
let ``OpCode tests`` (l, o, a, cs) = 
    let { OpCodes = opc; OpCodeGroups = opcx } = loadInstructionSet l
    opc.[o] |> should equal a
    (opc.Count, opcx.Count) |> should equal cs

let opCodeGroupTestData : seq<obj []> = 
    [ "GRP5/3\tCALL\tMp", ("GRP5", 3uy), 
      { OcName = "CALL"
        OcArgs = [| "Mp" |] }, (0, 1)
      "GRP3a/1 TEST\t Eb Gv", ("GRP3a", 1uy), 
      { OcName = "TEST"
        OcArgs = [| "Eb"; "Gv" |] }, (0, 1) ]
    |> Seq.map (fun (a, b, c, d) -> [| a; b; c; d |])

[<Theory>]
[<MemberData("opCodeGroupTestData")>]
let ``OpCode group tests`` (l, (on, oi), a, cs) = 
    let { OpCodes = opc; OpCodeGroups = opcx } = loadInstructionSet l
    opcx.[{ OcgName = on
            OcgIndex = oi }] |> should equal a
    (opc.Count, opcx.Count) |> should equal cs

[<Fact>]
let ``OpCode and OpCode group``() = 
    let { OpCodes = opc; OpCodeGroups = opcx } = loadInstructionSet "FC  CLD\nGRP2/7\tSAR"
    opc.[0xFCuy] |> should equal { OcName = "CLD"
                                   OcArgs = [||] }
    opcx.[{ OcgName = "GRP2"
            OcgIndex = 7uy }] |> should equal { OcName = "SAR"
                                                OcArgs = [||] }
    (opc.Count, opcx.Count) |> should equal (1, 1)

[<Fact>]
let ``rule with undefined opcode``() = 
    grammer.OpcRules.[0xF1] 
    |> should equal { OcType = OctNormal
                      OcId = 0x00
                      OcArgs = [||] }

[<Fact>]
let ``rule with 0 arg``() = 
    grammer.OpcRules.[0x3E] 
    |> should equal { OcType = OctNormal
                      OcId = 0x17
                      OcArgs = [||] }

[<Fact>]
let ``rule with 1 arg``() = 
    grammer.OpcRules.[0x1E] 
    |> should equal { OcType = OctNormal
                      OcId = 0x48
                      OcArgs = [| OcaSpecial(SpecialArgCode.DS) |] }

[<Fact>]
let ``rule with 2 arg``() = 
    grammer.OpcRules.[0x38] 
    |> should equal { OcType = OctNormal
                      OcId = 0x0E
                      OcArgs = 
                        [| OcaNormal(NormalArgCode.E ||| NormalArgCode.B)
                           OcaNormal(NormalArgCode.G ||| NormalArgCode.B) |] }

[<Fact>]
let ``rule with 1 arg extended opcode``() = 
    grammer.OpcRules.[0xF6] 
    |> should equal { OcType = OctExtension
                      OcId = 0x02
                      OcArgs = [| OcaNormal(NormalArgCode.E ||| NormalArgCode.B) |] }

[<Fact>]
let ``rule with 2 arg extended opcode``() = 
    grammer.OpcRules.[0xD0] 
    |> should equal { OcType = OctExtension
                      OcId = 0x01
                      OcArgs = 
                        [| OcaNormal(NormalArgCode.E ||| NormalArgCode.B)
                           OcaSpecial(SpecialArgCode.One) |] }

[<Fact>]
let ``rule with undefined extended``() = 
    grammer.OpcgRules.[1, 0x02] 
    |> should equal { OcType = OctNormal
                      OcId = 0x00
                      OcArgs = [||] }

[<Fact>]
let ``rule with extended opcode 0 args``() = 
    grammer.OpcgRules.[0, 0x05] 
    |> should equal { OcType = OctNormal
                      OcId = 0x1D
                      OcArgs = [||] }

[<Fact>]
let ``rule with extended opcode 1 args``() = 
    grammer.OpcgRules.[3, 0x05] 
    |> should equal { OcType = OctNormal
                      OcId = 0x08
                      OcArgs = [| OcaNormal(NormalArgCode.M ||| NormalArgCode.P) |] }

[<Fact>]
let ``rule with extended opcode 2 args``() = 
    grammer.OpcgRules.[0, 0x03] 
    |> should equal { OcType = OctNormal
                      OcId = 0x60
                      OcArgs = 
                        [| OcaNormal(NormalArgCode.E ||| NormalArgCode.W)
                           OcaNormal(NormalArgCode.I ||| NormalArgCode.W) |] }
