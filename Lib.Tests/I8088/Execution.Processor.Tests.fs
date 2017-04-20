module Lib.Chips.I8088.Excution.Processor.Tests

open FSharpx.State
open FsUnit.Xunit
open Lib
open Lib.Chips.I8088.Execution.Processor
open Lib.Chips.I8088
open global.Xunit

let mb = Common.mb

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``CMC tests`` fVal = 
    mb.Registers.Flags.[int (Flags.CF)] <- fVal
    eval (execCMC()) mb |> ignore
    mb.Registers.Flags.[int (Flags.CF)] |> should equal (not fVal)
