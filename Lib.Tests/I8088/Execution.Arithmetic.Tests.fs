module Lib.Chips.I8088.Excution.Arithmetic.Tests

open FSharpx
open FsCheck
open Lib
open Lib.Chips.I8088.Execution.Arithmetic
open Lib.Chips.I8088.Execution.Common
open Lib.Common
open Lib.Chips.I8088.InstructionSet
open Lib.Chips.I8088
open System
open YaFunTK
open global.Xunit

[<Theory>]
[<InlineData("AADD.tests.com", 42, 0x15e)>]
[<InlineData("AINC.tests.com", 30, 0x116)>]
[<InlineData("ASUB.tests.com", 40, 0x153)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount
