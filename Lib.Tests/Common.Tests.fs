module Lib.Common.Tests

open FsUnit.Xunit
open Xunit
open Lib

[<Theory>]
[<InlineData(12uy, 12)>]
[<InlineData(1uy, 1)>]
[<InlineData(0uy, 0)>]
[<InlineData(0xeeuy, -18)>]
[<InlineData(0xffuy, -1)>]
let ``signExtend tests`` (a : uint8) (b : int32) = 
    let a' = Common.signExtend a 
    let x = 0x7FFF |> uint16
    (int32)(x + a') |> should equal ((int32)x + b)
