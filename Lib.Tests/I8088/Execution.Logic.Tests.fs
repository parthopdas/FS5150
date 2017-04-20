module Lib.Chips.I8088.Excution.Logic.Tests

open Lib.Common
open global.Xunit

[<Theory>]
[<InlineData("LSHL.tests.com", 0x20, 0x119)>]
[<InlineData("LSHR.tests.com", 0x1a, 0xdd)>]
[<InlineData("LNOT.tests.com", 4, 0x33)>]
[<InlineData("LOR.tests.com", 24, 0x117)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount
