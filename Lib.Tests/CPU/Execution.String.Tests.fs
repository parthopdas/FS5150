module Lib.Excution.String.Tests

open FSharpx.State
open FsUnit.Xunit
open Lib
open Lib.CPU.Execution.Common
open Lib.CPU.Execution.String
open Lib.Domain.InstructionSet
open Lib.Domain.PC
open global.Xunit
open Lib.Common

[<Theory>]
[<InlineData("SSTOLOD.tests.com", 14, 0x146)>]
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
        mb.CPU.RepetitionType <- reptype
        mb.CPU.CX <- cx
        mb.CPU.Flags.[int(Flags.ZF)] <- false
        mb.CPU.Flags.[int(Flags.DF)] <- df
        mb.CPU.ES <- esdi.Segment
        mb.CPU.DI <- esdi.Offset
        mb.CPU.AX <- 0xdeadus
        mb.CPU.LogicalInstrStart <- csip.Segment @|@ 0x10us
        let init = (setCSIP csip) *> (writeWord16 v esdi)
        let wf = init *> SCASX.coreSCASW
        eval wf mb
        |> Option.fold (fun _ -> id) csip
        |> should equal csip'
        mb.CPU.ES @|@ mb.CPU.DI |> should equal esdi'
        mb.CPU.Flags.[int(Flags.ZF)] |> should equal zf
        mb.CPU.CX |> should equal cx'
