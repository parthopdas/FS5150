namespace Lib.CPU.Execution

module Arithmetic = 
    open FSharpx
    open FSharpx.Functional
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let flagAdd16 (v1 : Word16, v2 : Word16) = 
        let dst = (uint32) v1 + (uint32) v2
        (flagSZP16 ((uint16) dst)) *> (setFlag CF (dst &&& 0xFFFF0000ul <> 0ul)) 
        *> (setFlag OF (((dst ^^^ (uint32) v1) &&& (dst ^^^ (uint32) v2) &&& 0x8000ul) = 0x8000ul)) 
        *> (setFlag AF ((((uint32) v1 ^^^ (uint32) v2 ^^^ dst) &&& 0x10ul) = 0x10ul))
    
    let flagSub16 (v1 : Word16, v2 : Word16) = 
        let dst = (uint32) v1 - (uint32) v2
        (flagSZP16 ((uint16) dst)) *> (setFlag CF (dst &&& 0xFFFF0000ul <> 0ul)) 
        *> (setFlag OF (((dst ^^^ (uint32) v1) &&& ((uint32) v1 ^^^ (uint32) v2) &&& 0x8000ul) <> 0ul)) 
        *> (setFlag AF ((((uint32) v1 ^^^ (uint32) v2 ^^^ dst) &&& 0x10ul) <> 0ul))
    
    let opAdd16 v1v2 = 
        let res = v1v2 ||> (+)
        flagAdd16 v1v2 *> (res |> State.returnM)
    
    let opSub16 v1v2 = 
        let res = v1v2 ||> (-)
        flagSub16 v1v2 *> (res |> State.returnM)
    
    let execADD instr = 
        match instr.Args with
        | [ ArgRegister16 AX; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 AX <*> (c |> State.returnM)
             >>= opAdd16
             >>= setReg16 AX)
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
        | [ ArgRegister16 AX; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 AX <*> (c |> State.returnM) >>= flagSub16) *> ns
        | _ -> nyi instr
