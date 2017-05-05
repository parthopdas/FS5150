module Lib.Chips.I8259

open Lib.Chips.I8088.InstructionSet
open YaFunTK.Result

(*

Intel 8254 - Programmable Interrupt Controller

A ridiculously simple implementation. 

Here are the many things that don't work:
- Cannot be reinitialized
- IRQ priority not honored
- Nested interrupts unsupported (EOI clears ISR)
- SNGL mode only 
- Re-initialization not supported
- Multitude of other initializations and operations (refer data sheet).

References:
- http://www.brokenthorn.com/Resources/OSDevPic.html
- https://pdos.csail.mit.edu/6.828/2014/readings/hardware/8259A.pdf
- https://github.com/NeatMonster/Intel8086/blob/master/src/fr/neatmonster/ibmpc/Intel8259.java
- https://github.com/rubbermallet/fake86/blob/master/src/fake86/i8259.c
- https://github.com/Henne/dosbox-svn/blob/master/src/hardware/pic.cpp

*)

(*
TODO:
- Interrupts cannot arrive between pairs of instruction like pop CS, etc.
- Move WordXX to common
*)

type Port = Word16

type Data = Word16

type ICW = Word16

[<Literal>]
let PIC1CTRL : Port = 0x20us

[<Literal>]
let PIC1DATA : Port = 0x21us

[<Literal>]
let BitA0 = 0b100000000us

[<Literal>]
let BitD0 = 0b000000001us

[<Literal>]
let BitD1 = 0b000000010us

[<Literal>]
let BitD3 = 0b000001000us

[<Literal>]
let BitD4 = 0b000010000us

[<Literal>]
let BitD5 = 0b000100000us

[<Literal>]
let PICREADIRR = 0x0aus

[<Literal>]
let PICREADISR = 0x0bus

type InitState = 
    | Uninitialized
    | ICW1Received
    | ICW2Received
    | Initialized

// TODO: Why is this word16? Should it not be Word8?
type I8259 = 
    { IMR : Word16
      IRR : Word16
      ISR : Word16
      InitState : InitState
      IVTBA : Word16
      ReadISR : bool }
    static member Zero =
        { IMR = 0us
          IRR = 0us
          ISR = 0us
          InitState = Uninitialized
          IVTBA = 0us
          ReadISR = false }

/// Is bit(s) off in word
let inline (?-) b w = w &&& b <> b
/// Is bit(s) on in word
let inline (?+) b w = w &&& b = b
/// Index to Bit: 8 -> 0b00001000
let i2B = (<<<) 1us
/// Bit to index: 0b00001000 -> 8
let b2I x =
    let rec loop i s =
        if s ?+ x then i
        else loop (i + 1us) (s <<< 1) 
    loop 0us 1us

let inline readInitState f it =
    it.InitState |> f
    it |> Success

let write p d it =
    match p with
    | PIC1CTRL ->
        match d with
        // ICW1 
        | _ when BitA0 ?- d && BitD4 ?+ d ->
            if BitD0 ?- d then
                Failure "ICW1 must have IC4 bit on"
            else if  BitD1 ?- d then
                Failure "ICW1 must have SNGL bit on"
            else
                Success { it with InitState = ICW1Received }

        // OCW2 - EOI
        | _ when BitD5 ?+ d -> 
            let isr = 
                seq { for i in 0..7 -> (i2B i) } 
                |> Seq.filter (fun i -> i ?+ it.ISR)
                |> Seq.fold (^^^) it.ISR
            Success { it with ISR = isr }

        // OCW3 - Switch reading to IxR
        | _ when BitD3 ?+ d -> 
            { it with ReadISR = (BitD0 ||| BitD1) ?+ d } |> Success
        | _ -> it |> Success
    | PIC1DATA -> 
        match it.InitState with
        | Uninitialized -> it |> Success
        | ICW1Received -> 
            { it with InitState = ICW2Received; IVTBA = d } |> Success
        | ICW2Received(_) when BitD0 ?- d -> 
            Failure "ICW4 must have iPM bit on"
        | ICW2Received -> 
            { it with InitState = Initialized } |> Success
        | Initialized(_)  -> 
            { it with IMR = d } |> Success
    | _ -> Prelude.undefined

let read p it =
    match p with
    | PIC1CTRL -> 
        (if it.ReadISR then it.ISR else it.IRR) |> Success
    | PIC1DATA -> 
        it.IMR |> Success
    | _ -> Prelude.undefined

let activateIRQ irq it =
    let it = 
        { it with IRR = it.IRR ||| i2B irq }
    it |> Success 

let activateINTA it =
    let irqs = it.IRR &&& ~~~it.IMR
    let lowest =
        seq { for i in 0..7 -> (i2B i) } 
        |> Seq.filter (fun i -> i ?+ irqs)
        |> Seq.tryPick Some
        |> Option.fold (fun _ e -> e) 0us

    if (lowest <> 0us) then
        let it =
            { it with 
                IRR = it.IRR ^^^ lowest
                ISR = it.ISR ||| lowest }
        (it.IVTBA + b2I lowest, it) |> Success
    else
        (0us, it) |> Success
        