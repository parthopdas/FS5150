module Lib.Excution.Common.Tests

open Lib.CPU.Execution.Common
open FsUnit.Xunit

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
