module Lib.Excution.Control.Tests

open FSharpx.State
open FsUnit.Xunit
open Lib
open Lib.CPU.Execution.Common
open Lib.CPU.Execution.Control

let mb = Common.mb

module CALL = 
    [<Xunit.Fact>]
    let ``Core CALL tests``() = 
        let csip = 0x0us @|@ 0x10us
        let sssp = 0x0us @|@ 0x20us
        let w16 = 0x1111us
        let wf = setCSIP csip *> setSSSP sssp *> coreCALL 0x10us w16
        eval wf mb |> should equal (csip |++ 0x10us |++ w16)
        eval (getSSSP >>= readWord16) mb |> should equal csip.Offset
