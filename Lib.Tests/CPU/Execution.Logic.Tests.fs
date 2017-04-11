﻿module Lib.Excution.Logic.Tests

open Lib.Common
open global.Xunit

[<Theory>]
[<InlineData("LSHL.tests.com", 0x20, 0x10f)>]
[<InlineData("LSHR.tests.com", 0x1a, 0xd3)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount
