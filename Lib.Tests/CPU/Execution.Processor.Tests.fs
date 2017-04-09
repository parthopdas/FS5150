module Lib.Excution.Processor.Tests

open FSharpx.State
open FsUnit.Xunit
open Lib
open Lib.CPU.Execution.Processor
open Lib.Domain.PC
open global.Xunit

let mb = Common.mb

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``CMC tests`` fVal = 
    mb.CPU.Flags.[int (Flags.CF)] <- fVal
    eval (execCMC()) mb |> ignore
    mb.CPU.Flags.[int (Flags.CF)] |> should equal (not fVal)
