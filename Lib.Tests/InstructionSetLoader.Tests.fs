module Lib.InstructionSetLoader.Tests

open FsUnit.Xunit
open Lib.Domain.InstructionSet
open Lib.InstructionSetLoader
open Xunit


[<Fact>]
let ``Ignore comments and empty lines`` () = 
    let lines = "\r\n*  asda\n"
    let is = loadInstructionSet lines
    { OpCodes = Map.empty; OpCodeGroups = Map.empty }  |> should equal is

let opCodetestData : seq<obj []> = 
    [ "DB\t--", 0xDBuy, [ "--" ], (1, 0)
      "20\tAND \t\tEb  Gb", 0x20uy, [ "AND"; "Eb"; "Gb" ], (1, 0) ]
    |> Seq.map (fun (a, b, c, d) -> [| a; b; c; d |])

[<Theory>]
[<MemberData("opCodetestData")>]
let ``OpCode tests`` (l, o, a, cs) = 
    let { OpCodes = opc; OpCodeGroups = opcx } = loadInstructionSet l
    opc.[o] |> should equal a
    (opc.Count, opcx.Count) |> should equal cs

let opCodeGroupTestData : seq<obj []> = 
    [ "GRP5/3\tCALL\tMp", ("GRP5", 3uy), [ "CALL"; "Mp" ], (0, 1)
      "GRP3a/1 TEST\t Eb Gv", ("GRP3a", 1uy), [ "TEST"; "Eb"; "Gv" ], (0, 1) ]
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
    opc.[0xFCuy] |> should equal [ "CLD" ]
    opcx.[{ OcgName = "GRP2"
            OcgIndex = 7uy }] |> should equal [ "SAR" ]
    (opc.Count, opcx.Count) |> should equal (1, 1)
