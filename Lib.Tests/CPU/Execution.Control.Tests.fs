module Lib.Excution.Control.Tests

open Lib.Common
open global.Xunit

[<Fact>]
let ``CALL-RET Tests``() = 
    let mb = createMB "CALL-RET.tests.com"
    
    runTestFromCOMFile mb

    verifyAfterTestFromCOMFile mb 0x75L 6us
