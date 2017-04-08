module Lib.Excution.Logic.Tests

open Lib.Common
open global.Xunit

[<Theory>]
[<InlineData("SHL.tests.com", 0x20us, 0x11fL)>]
[<InlineData("SHR.tests.com", 0x1aus, 0xe3L)>]
let ``COM.Tests`` testName tCount iCount = 
    let mb = createMB testName
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb iCount tCount
