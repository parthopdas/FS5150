module Lib.Chips.I8088.Excution.String.Tests

open FSharpx.State
open FsUnit.Xunit
open Lib
open Lib.Chips.I8088.Execution.Common
open Lib.Chips.I8088.Execution.String
open Lib.Chips.I8088.InstructionSet
open Lib.Chips.I8088
open global.Xunit
open Lib.Common

[<Theory>]
[<InlineData("SSTOLOD.tests.com", 14, 0x146)>]
[<InlineData("SMOVX.tests.com", 8, 0xc9)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount

let mb = Common.mb
let esdi = 0x1us @|@ 0us
let csip = 0x10us @|@ 0us

module SCASX = 
    let ``Core SCASX tests data`` : obj array seq = 
        [| ((WhileZero, 0x0us, false, 0xbaadus), (csip, 0x1us @|@ 0us, 0x0us, false))
           ((WhileNotZero, 0x0us, true, 0xdeadus), (csip, 0x1us @|@ 0us, 0x0us, false))
           ((NoRepetition, 0x0us, true, 0xdeadus), (csip, 0x1us @|@ 0xfffeus, 0x0us, true))
           ((NoRepetition, 0x1us, false, 0xdeadus), (csip, 0x1us @|@ 0x2us, 0x1us, true))
           ((WhileZero, 0x1us, false, 0xdeadus), (0x10us @|@ 0x10us, 0x1us @|@ 0x2us, 0x0us, true))
           ((WhileZero, 0x1us, true, 0xbaadus), (csip, 0x1us @|@ 0xfffeus, 0x0us, false))
           ((WhileNotZero, 0x1us, true, 0xdeadus), (csip, 0x1us @|@ 0xfffeus, 0x0us, true))
           ((WhileNotZero, 0x1us, false, 0xbaadus), (0x10us @|@ 0x10us, 0x1us @|@ 0x2us, 0x0us, false)) |]
        |> Seq.map (fun (a, b) -> 
               [| box a
                  box b |])
    
    [<Xunit.Theory>]
    [<Xunit.MemberData("Core SCASX tests data")>]
    let ``Core SCASX tests`` ((reptype : RepetitionType, cx : Word16, df : bool, v : Word16), 
                              (csip' : Address, esdi' : Address, cx' : Word16, zf : bool)) : unit = 
        mb.Registers.RepetitionType <- reptype
        mb.Registers.CX <- cx
        mb.Registers.Flags.[int(Flags.ZF)] <- false
        mb.Registers.Flags.[int(Flags.DF)] <- df
        mb.Registers.ES <- esdi.Segment
        mb.Registers.DI <- esdi.Offset
        mb.Registers.AX <- 0xdeadus
        mb.Registers.LogicalInstrStart <- csip.Segment @|@ 0x10us
        let init = (setCSIP csip) *> (writeWord16 v esdi)
        let wf = init *> SCASX.coreSCASW
        eval wf mb
        |> Option.fold (fun _ -> id) csip
        |> should equal csip'
        mb.Registers.ES @|@ mb.Registers.DI |> should equal esdi'
        mb.Registers.Flags.[int(Flags.ZF)] |> should equal zf
        mb.Registers.CX |> should equal cx'
