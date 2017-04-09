module Lib.Excution.Data.Tests

open FSharpx.State
open FsUnit.Xunit
open Lib
open Lib.CPU.Execution.Common
open Lib.CPU.Execution.Data
open Lib.Domain.InstructionSet
open Lib.Domain.PC
open global.Xunit
open Lib.Common

[<Theory>]
[<InlineData("PUSHX-POPX.tests.com", 4, 0x75)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount

let mb = Common.mb
let esdi = 0x1us @|@ 0us
let csip = 0x10us @|@ 0us

module STOSX = 
    let ``Core STOSX tests data`` : obj array seq = 
        [| ((Some WhileZero, 0x0us, false), (csip, 0x1us @|@ 0us, 0xbaadus, 0x0us))
           ((None, 0x0us, true), (csip, 0x1us @|@ 0xfffeus, 0xdeadus, 0x0us))
           ((None, 0x1us, false), (csip, 0x1us @|@ 0x2us, 0xdeadus, 0x1us))
           ((Some WhileNotZero, 0x1us, false), (0x10us @|@ 0x10us, 0x1us @|@ 0x2us, 0xdeadus, 0x0us)) |]
        |> Seq.map (fun (a, b) -> 
               [| box a
                  box b |])
    
    [<Xunit.Theory>]
    [<Xunit.MemberData("Core STOSX tests data")>]
    let ``Core STOSX tests`` ((reptype : RepetitionType option, cx : Word16, df : bool), 
                              (csip' : Address, esdi' : Address, v : Word16, cx' : Word16)) : unit = 
        mb.CPU.RepetitionType <- reptype
        mb.CPU.CX <- cx
        mb.CPU.Flags.[int(Flags.DF)] <- df
        mb.CPU.ES <- esdi.Segment
        mb.CPU.DI <- esdi.Offset
        mb.CPU.AX <- 0xdeadus
        mb.CPU.LogicalInstrStart <- csip.Segment @|@ 0x10us
        let init = (setCSIP csip) *> (writeWord16 0xbaadus esdi)
        let wf = init *> coreSTOSW
        eval wf mb
        |> Option.fold (fun _ -> id) csip
        |> should equal csip'
        mb.CPU.ES @|@ mb.CPU.DI |> should equal esdi'
        eval (readWord16 (esdi)) mb |> should equal v
        mb.CPU.CX |> should equal cx'

module SCASX = 
    let ``Core SCASX tests data`` : obj array seq = 
        [| ((Some WhileZero, 0x0us, false, 0xbaadus), (csip, 0x1us @|@ 0us, 0x0us, false))
           ((Some WhileNotZero, 0x0us, true, 0xdeadus), (csip, 0x1us @|@ 0us, 0x0us, false))
           ((None, 0x0us, true, 0xdeadus), (csip, 0x1us @|@ 0xfffeus, 0x0us, true))
           ((None, 0x1us, false, 0xdeadus), (csip, 0x1us @|@ 0x2us, 0x1us, true))
           ((Some WhileZero, 0x1us, false, 0xdeadus), (0x10us @|@ 0x10us, 0x1us @|@ 0x2us, 0x0us, true))
           ((Some WhileZero, 0x1us, true, 0xbaadus), (csip, 0x1us @|@ 0xfffeus, 0x0us, false))
           ((Some WhileNotZero, 0x1us, true, 0xdeadus), (csip, 0x1us @|@ 0xfffeus, 0x0us, true))
           ((Some WhileNotZero, 0x1us, false, 0xbaadus), (0x10us @|@ 0x10us, 0x1us @|@ 0x2us, 0x0us, false)) |]
        |> Seq.map (fun (a, b) -> 
               [| box a
                  box b |])
    
    [<Xunit.Theory>]
    [<Xunit.MemberData("Core SCASX tests data")>]
    let ``Core SCASX tests`` ((reptype : RepetitionType option, cx : Word16, df : bool, v : Word16), 
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
        let wf = init *> coreSCASW
        eval wf mb
        |> Option.fold (fun _ -> id) csip
        |> should equal csip'
        mb.CPU.ES @|@ mb.CPU.DI |> should equal esdi'
        mb.CPU.Flags.[int(Flags.ZF)] |> should equal zf
        mb.CPU.CX |> should equal cx'
