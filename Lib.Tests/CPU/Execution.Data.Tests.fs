module Lib.Excution.Data.Tests

open FSharpx.State
open FsUnit.Xunit
open Lib
open Lib.CPU.Execution.Common
open Lib.CPU.Execution.Data
open Lib.Domain.InstructionSet
open Lib.Domain.PC

let mb = Common.mb
let esdi = 0x1us @|@ 0us
let csip = 0x10us @|@ 0us

let ``Core STOSW tests data`` : obj array seq = 
    [| ((Some TillZero, 0x0us, false), (0x10us @|@ 0us, 0x1us @|@ 0us, 0xbaadus, 0x0us))
       ((None, 0x0us, true), (0x10us @|@ 0us, 0x1us @|@ 0xfffeus, 0xdeadus, 0x0us))
       ((None, 0x1us, false), (0x10us @|@ 0us, 0x1us @|@ 0x2us, 0xdeadus, 0x1us))
       ((Some TillNotZero, 0x1us, false), (0x10us @|@ 0x10us, 0x1us @|@ 0x2us, 0xdeadus, 0x0us)) |]
    |> Seq.map (fun (a, b) -> 
           [| box a
              box b |])

[<Xunit.Theory>]
[<Xunit.MemberData("Core STOSW tests data")>]
let ``Core STOSW tests`` ((reptype : RepetitionType option, cx : Word16, df : bool), 
                          (csip' : Address, esdi' : Address, v : Word16, cx' : Word16)) : unit = 
    mb.CPU.RepetitionType <- reptype
    mb.CPU.CX <- cx
    mb.CPU.Flags.[DF] <- df
    mb.CPU.ES <- esdi.Segment
    mb.CPU.DI <- esdi.Offset
    mb.CPU.AX <- 0xdeadus
    mb.CPU.LogicalInstrStart <- csip.Segment @|@ 0x10us
    let init = (setCSIP csip) *> (writeWord16 0xbaadus esdi)
    let wf = init *> coreSTOSX (getReg16 AX) writeWord16 2us 
    eval wf mb
    |> Option.fold (fun _ -> id) csip
    |> should equal csip'
    mb.CPU.ES @|@ mb.CPU.DI |> should equal esdi'
    eval (readWord16 (esdi)) mb |> should equal v
    mb.CPU.CX |> should equal cx'
