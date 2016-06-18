module Lib.Excution.Common.Tests

open FSharpx.Functional
open FsUnit.Xunit
open Lib.CPU.Execution.Common
open Lib.Domain.InstructionSet
open FSharpx.State

let mb = Lib.Common.mb

[<Xunit.Fact>]
let ``Word8 to Word16 tests`` () =
    !<>0xdeuy |> should equal 0x00deus

[<Xunit.Fact>]
let ``Word16 to Word8 tests`` () =
    !><0xdeadus |> should equal 0xaduy

[<Xunit.Fact>]
let ``make w16 from 2 w8`` () =
    0xbauy +|+ 0xaduy |> should equal 0xbaadus

[<Xunit.Fact>]
let ``get high byte tests`` () =
    0xdeadus |> getHiByte |> should equal 0xdeuy

[<Xunit.Fact>]
let ``get low byte tests`` () =
    0xfeedus |> getLoByte |> should equal 0xeduy

[<Xunit.Fact>]
let ``set high byte tests`` () =
    setHiByte 0xdeadus 0xbauy |> should equal 0xbaadus

[<Xunit.Fact>]
let ``set low byte tests`` () =
    setLoByte 0xdeadus 0xbauy |> should equal 0xdebaus

[<Xunit.Theory>]
[<Xunit.InlineData(0x0000us, 0x000Fus, 0x0000Ful)>]
[<Xunit.InlineData(0xFFFFus, 0xFFFFus, 0x0FFEFul)>]
let ``flatten tests`` (s, o, fa : uint32) =
    createAddr s o |> flatten |> should equal fa

[<Xunit.Theory>]
[<Xunit.InlineData(0x0000us, 0x000Fus, 1us, 0x0000us, 0x0010us)>]
[<Xunit.InlineData(0xDEADus, 0xFFFFus, 0xFFFFus, 0xDEADus, 0xFFFEus)>]
let ``|++ tests`` (s1, o1, n, s2, o2) =
    createAddr s1 o1 
    |> Prelude.flip (|++) n 
    |> should equal (createAddr s2 o2)

[<Xunit.Theory>]
[<Xunit.InlineData(0x0000us, 0x000Fus, 1us, 0x0000us, 0x000Eus)>]
[<Xunit.InlineData(0xDEADus, 0x0000us, 0xFFFFus, 0xDEADus, 0x0001us)>]
let ``|-- tests`` (s1, o1, n, s2, o2) =
    createAddr s1 o1 
    |> Prelude.flip (|--) n 
    |> should equal (createAddr s2 o2)

[<Xunit.Fact>]
let ``set CSIP tests`` () =
    let tos = 
        { Segment = 0x10us
          Offset = 0x02us }

    eval (setCSIP tos) mb
    createAddr mb.CPU.CS mb.CPU.IP |> should equal tos

[<Xunit.Fact>]
let ``get CSIP tests`` () =
    mb.CPU.CS <- 0xBAADus
    mb.CPU.IP <- 0xF00Dus

    let a = eval getCSIP mb
    a |> should equal (createAddr 0xBAADus 0xF00Dus)

[<Xunit.Fact>]
let ``set SSSP tests`` () =
    let tos = 
        { Segment = 0x10us
          Offset = 0x02us }

    eval (setSSSP tos) mb
    createAddr mb.CPU.SS mb.CPU.SP |> should equal tos

[<Xunit.Fact>]
let ``get SSSP tests`` () =
    mb.CPU.SS <- 0xBAADus
    mb.CPU.SP <- 0xF00Dus

    let a = eval getSSSP mb
    a |> should equal (createAddr 0xBAADus 0xF00Dus)
