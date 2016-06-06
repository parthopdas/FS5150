namespace Lib.CPU.Execution

module Arithmetic = 
    open FSharpx
    open FSharpx.Functional
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    // TODO: Combine the methods below using duck typing
    let flagAdd16 (v1 : Word16, v2 : Word16) = 
        let dst = (uint32) v1 + (uint32) v2
        (flagSZP16 ((uint16) dst)) *> (setFlag CF (dst &&& 0xFFFF0000ul <> 0ul)) 
        *> (setFlag OF (((dst ^^^ (uint32) v1) &&& (dst ^^^ (uint32) v2) &&& 0x8000ul) = 0x8000ul)) 
        *> (setFlag AF ((((uint32) v1 ^^^ (uint32) v2 ^^^ dst) &&& 0x10ul) = 0x10ul))
    
    let flagAdd8 (v1 : Word8, v2 : Word8) = 
        let dst = (uint16) v1 + (uint16) v2
        (flagSZP16 ((uint16) dst)) *> (setFlag CF (dst &&& 0xFF00us <> 0us)) 
        *> (setFlag OF (((dst ^^^ (uint16) v1) &&& (dst ^^^ (uint16) v2) &&& 0x80us) = 0x80us)) 
        *> (setFlag AF ((((uint16) v1 ^^^ (uint16) v2 ^^^ dst) &&& 0x10us) = 0x10us))
    
    let flagSub16 (v1 : Word16, v2 : Word16) = 
        let dst = (uint32) v1 - (uint32) v2
        (flagSZP16 ((uint16) dst)) *> (setFlag CF (dst &&& 0xFFFF0000ul <> 0ul)) 
        *> (setFlag OF (((dst ^^^ (uint32) v1) &&& ((uint32) v1 ^^^ (uint32) v2) &&& 0x8000ul) <> 0ul)) 
        *> (setFlag AF ((((uint32) v1 ^^^ (uint32) v2 ^^^ dst) &&& 0x10ul) <> 0ul))
    
    let flagSub8 (v1 : Word8, v2 : Word8) = 
        let dst = (uint16) v1 - (uint16) v2
        (flagSZP16 ((uint16) dst)) *> (setFlag CF (dst &&& 0xFF00us <> 0us)) 
        *> (setFlag OF (((dst ^^^ (uint16) v1) &&& ((uint16) v1 ^^^ (uint16) v2) &&& 0x80us) <> 0us)) 
        *> (setFlag AF ((((uint16) v1 ^^^ (uint16) v2 ^^^ dst) &&& 0x10us) <> 0us))
    
    let opAdd16 v1v2 = 
        let res = v1v2 ||> (+)
        flagAdd16 v1v2 *> (res |> State.returnM)
    
    let opAdd8 v1v2 = 
        let res = v1v2 ||> (+)
        flagAdd8 v1v2 *> (res |> State.returnM)
    
    let opSub16 v1v2 = 
        let res = v1v2 ||> (-)
        flagSub16 v1v2 *> (res |> State.returnM)
    
    let execADD instr = 
        match instr.Args with
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM)
             >>= opAdd16
             >>= setReg16 r)
            *> ns
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM)
             >>= opAdd8
             >>= setReg8 r)
            *> ns
        | _ -> nyi instr
    
    let execINC instr = 
        match instr.Args with
        | [ ArgRegister16 AX ] -> 
            let add1 = 
                Prelude.tuple2 <!> getReg16 AX <*> (1us |> State.returnM)
                >>= opAdd16
                >>= setReg16 AX
            (getFlag CF <* add1 >>= setFlag CF) *> ns
        | [ ArgRegister8 AL ] -> 
            let add1 = 
                Prelude.tuple2 <!> getReg8 AL <*> (1uy |> State.returnM)
                >>= opAdd8
                >>= setReg8 AL
            (getFlag CF <* add1 >>= setFlag CF) *> ns
        | _ -> nyi instr
    
    let execSUB instr = 
        match instr.Args with
        | [ ArgRegister16 AX; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 AX <*> (c |> State.returnM)
             >>= opSub16
             >>= setReg16 AX)
            *> ns
        | _ -> nyi instr
    
    let execCMP instr = 
        match instr.Args with
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM) >>= flagSub16) *> ns
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM) >>= flagSub8) *> ns
        | [ ArgRegister16 r; ArgDereference dref ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            (Prelude.tuple2 <!> getReg16 r <*> w16 >>= flagSub16) *> ns
        | _ -> nyi instr
