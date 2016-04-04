module Lib.Tests

open Xunit
open FsUnit.Xunit

[<Fact>]
let xx () =
    let x = new Lib.Class1()
    x.X |> should equal "F#"