module Lib.Excution.Data.Tests

open Lib
open global.Xunit
open Lib.Common

[<Theory>]
[<InlineData("DPUSHX-POPX.tests.com", 4, 0x6f)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount
