module Lib.Excution.Control.Tests

open Lib.Common
open global.Xunit


[<Theory>]
[<InlineData("CCALL-RET.tests.com", 6, 0x6f)>]
[<InlineData("CLOOPX.tests.com", 9, 0xd8)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount
