module Lib.Chips.I8259

open global.Xunit
open Lib.Chips.I8259
open FsUnit.Xunit
open YaFunTK.Result

type WOpResult = Result<I8259, string>
type ROpResult = Result<Data, string>

let inline getOrDefault d = Result.fold id (Prelude.ct d) 

[<Fact>]
let ``InitCmd - Error out if ICW1 does not have IC4 bit on`` () = 
    I8259.Zero 
    |> write PIC1CTRL 0xfeus
    |> should equal (WOpResult.Failure "ICW1 must have IC4 bit on")

[<Fact>]
let ``InitCmd - Error out if ICW1 does not have SNGL bit on`` () = 
    I8259.Zero 
    |> write PIC1CTRL 0x11us
    |> should equal (WOpResult.Failure "ICW1 must have SNGL bit on")

[<Fact>]
let ``InitCmd - ICW1 not accepted if sent to DATA port`` () = 
    I8259.Zero 
    |> (write PIC1DATA 0x13us
        >=> readInitState (fun is -> is |> should equal Uninitialized)) 
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``InitCmd - ICW1 accepted`` () =
    I8259.Zero 
    |> (write PIC1CTRL 0x33us 
        >=> readInitState (fun is -> is |> should equal ICW1Received)) 
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>] 
let ``InitCmd - ICW2 not accepted if sent to CTRL port`` () = 
    I8259.Zero 
    |> (write PIC1CTRL 0x13us
        >=> write PIC1CTRL 0x10us
        >=> readInitState (fun is -> is |> should not' <| equal ICW2Received)) 
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``InitCmd - ICW2 accepted`` () = 
    let r = 
        I8259.Zero 
        |> (write PIC1CTRL 0x13us
            >=> write PIC1DATA 0x10us)
        |> getOrDefault I8259.Zero
    
    r.InitState |> should equal ICW2Received
    r.IVTBA |> should equal 0x10us

[<Fact>]
let ``InitCmd - Error out if ICW4 does not have iPM bit on`` () = 
    I8259.Zero 
    |> (write PIC1CTRL 0x13us
        >=> write PIC1DATA 0x10us
        >=> write PIC1DATA 0x00us) 
    |> should equal (WOpResult.Failure "ICW4 must have iPM bit on")

[<Fact>]
let ``InitCmd - ICW4 not accepted if sent to CTRL port`` () = 
    I8259.Zero 
    |> (write PIC1CTRL 0x13us
        >=> write PIC1DATA 0x10us
        >=> write PIC1CTRL 0x03us
        >=> readInitState (fun is -> is |> should not' <| equal Initialized)) 
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``InitCmd - ICW4 accepted`` () = 
    I8259.Zero 
    |> (write PIC1CTRL 0x13us
        >=> write PIC1DATA 0x10us
        >=> write PIC1DATA 0x09us 
        >=> readInitState (fun is -> is |> should equal Initialized)) 
    |> getOrDefault I8259.Zero
    |> ignore

let init8259 () = 
    I8259.Zero
    |> (write PIC1CTRL 0x13us
        >=> write PIC1DATA 0x08us
        >=> write PIC1DATA 0x09us)

let maskIRQ irq = write PIC1DATA (i2B(int(irq)))

let readIRR = write PIC1CTRL PICREADIRR
let readISR = write PIC1CTRL PICREADISR

let inline readIXR f it =
    it |> read PIC1CTRL |> getOrDefault 0xffffus |> f
    it |> Success

let sendEOI = write PIC1CTRL 0x20us

[<Fact>]
let ``RegIO - Write and read IMR from DATA port`` () = 
    () 
    |> (init8259
        >=> write PIC1DATA 0xdeadus
        >=> read PIC1DATA) 
    |> getOrDefault 0us
    |> should equal 0xdeadus

[<Fact>]
let ``RegIO - IMR cannot be written to CTRL port`` () = 
    () 
    |> (init8259
        >=> write PIC1DATA 0xbeefus
        >=> write PIC1CTRL 0xdec7us
        >=> read PIC1DATA) 
    |> getOrDefault 0us
    |> should equal 0xbeefus

[<Fact>]
let ``RegIO - IMR cannot be read from CTRL port`` () = 
    () 
    |> (init8259
        >=> write PIC1DATA 0xbeefus
        >=> read PIC1CTRL) 
    |> getOrDefault 0us
    |> should equal 0us

[<Fact>]
let ``activateIRQ - activateIRQs register masked and unmasked IRQs for request`` () =
    () 
    |> (init8259
        >=> maskIRQ 1
        >=> activateIRQ 1
        >=> activateIRQ 3
        >=> readIRR >=> readIXR (fun irr -> irr |> should equal 10us)) 
    |> getOrDefault I8259.Zero
    |> ignore

let inline activateINTA f it =
    let ivta, it = it |> activateINTA |> getOrDefault (0xffffus, I8259.Zero)
    f ivta
    it |> Success

[<Fact>]
let ``activateINTA - activateIRQs activateINTAs register lowest IRQ for service`` () = 
    () 
    |> (init8259
        >=> activateIRQ 0
        >=> activateIRQ 5
        >=> activateINTA(fun ivta -> ivta |> should equal 8us)
        >=> readISR >=> readIXR (fun isr -> isr |> should equal (i2B 0))
        >=> readIRR >=> readIXR (fun irr -> irr |> should equal (i2B 5)))
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``activateINTA - activateINTAs without IRQ request is noop and returns 0`` () = 
    () 
    |> (init8259
        >=> activateINTA(fun ivta -> ivta |> should equal 0us)
        >=> readISR >=> readIXR (fun isr -> isr |> should equal 0us)
        >=> readIRR >=> readIXR (fun irr -> irr |> should equal 0us))
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``EOI - activateIRQs activateINTAs EOI unregisters all IRQs from service`` () = 
    () 
    |> (init8259
        >=> activateIRQ 1
        >=> activateIRQ 2
        >=> activateIRQ 3
        >=> activateINTA (fun ivta -> ivta |> should equal 9us)
        >=> activateINTA (fun ivta -> ivta |> should equal 10us)
        >=> sendEOI
        >=> readISR >=> readIXR (fun isr -> isr |> should equal 0us)
        >=> readIRR >=> readIXR (fun irr -> irr |> should equal (i2B 3)))
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``EOI - Just EOI is noop`` () =
    () 
    |> (init8259
        >=> maskIRQ 1
        >=> activateIRQ 1
        >=> activateIRQ 3
        >=> sendEOI
        >=> readISR >=> readIXR (fun isr -> isr |> should equal 0us)
        >=> readIRR >=> readIXR (fun irr -> irr |> should equal (i2B 1 + i2B 3)))
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``EOI - EOI to DATA port does not work`` () = 
    () 
    |> (init8259
        >=> activateIRQ 1
        >=> activateINTA ignore
        >=> write PIC1DATA 0x20us
        >=> readISR >=> readIXR (fun isr -> isr |> should equal (i2B 1))
        >=> readIRR >=> readIXR (fun irr -> irr |> should equal 0us))
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``IMR - activateIRQs activateINTAs register unmasked IRQs for service`` () =
    () 
    |> (init8259
        >=> maskIRQ 0
        >=> activateIRQ 0
        >=> activateIRQ 5
        >=> activateINTA ignore
        >=> readISR >=> readIXR (fun isr -> isr |> should equal (i2B 5))
        >=> readIRR >=> readIXR (fun irr -> irr |> should equal (i2B 0)))
    |> getOrDefault I8259.Zero
    |> ignore

[<Fact>]
let ``IXR - Switch and read ISR IRR default IRR`` () =
    () 
    |> (init8259
        >=> activateIRQ 0
        >=> activateIRQ 5
        >=> activateINTA ignore
        >=> readIXR (fun irr -> irr |> should equal (i2B 5))
        >=> readISR >=> readIXR (fun isr -> isr |> should equal (i2B 0))
        >=> readIRR >=> readIXR (fun irr -> irr |> should equal (i2B 5)))
    |> getOrDefault I8259.Zero
    |> ignore
