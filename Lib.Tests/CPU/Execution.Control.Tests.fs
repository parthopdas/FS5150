module Lib.Excution.Control.Tests

open Lib.Common
open global.Xunit


[<Theory>]
[<InlineData("CALL-RET.tests.com", 6, 0x75)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount
