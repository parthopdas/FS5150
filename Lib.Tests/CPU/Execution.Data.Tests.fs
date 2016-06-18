module Lib.Excution.Data.Tests

open FSharpx.State
open FsUnit.Xunit
open Lib.CPU.Execution.Common
open Lib.CPU.Execution.Data
open Lib.Domain.InstructionSet

let mb = Lib.Common.mb

module PUSH = 
    [<Xunit.Fact>]
    let ``Core PUSH``() = 
        let tos = 
            { Segment = 0x10us
              Offset = 0x02us }
        
        let wf = (setSSSP tos) *> (corePUSH 0xdeadus) *> readWord16 (tos |-- 2us)
        let w16 = eval wf mb
        w16 |> should equal 0xdeadus
