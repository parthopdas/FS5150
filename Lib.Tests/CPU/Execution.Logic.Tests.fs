module Lib.Excution.Logic.Tests

open Lib.Common
open global.Xunit

[<Theory>]
[<InlineData("SHL.tests.com", 0x20, 0x11f)>]
[<InlineData("SHR.tests.com", 0x1a, 0xe3)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb tCount iCount
