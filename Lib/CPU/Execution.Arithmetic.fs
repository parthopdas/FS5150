namespace Lib.CPU.Execution

module Arithmetic = 
    open YaFunTK
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open Lib
    
    let flagAdd16 (v1 : Word16, v2 : Word16) = 
        let dst = (uint32) v1 + (uint32) v2
        (flagSZP16 ((uint16) dst)) 
        *> (setFlag CF (dst &&& 0xFFFF0000ul <> 0ul)) 
        *> (setFlag OF (((dst ^^^ (uint32) v1) &&& (dst ^^^ (uint32) v2) &&& 0x8000ul) = 0x8000ul)) 
        *> (setFlag AF ((((uint32) v1 ^^^ (uint32) v2 ^^^ dst) &&& 0x10ul) = 0x10ul))
    
    let flagAdd8 (v1 : Word8, v2 : Word8) = 
        let dst = (uint16) v1 + (uint16) v2
        (flagSZP8 ((uint8) dst)) 
        *> (setFlag CF (dst &&& 0xFF00us <> 0us)) 
        *> (setFlag OF (((dst ^^^ (uint16) v1) &&& (dst ^^^ (uint16) v2) &&& 0x80us) = 0x80us)) 
        *> (setFlag AF ((((uint16) v1 ^^^ (uint16) v2 ^^^ dst) &&& 0x10us) = 0x10us))
    
    let inline opAdd16 v1v2 = 
        let res = v1v2 ||> (+)
        flagAdd16 v1v2 *> (res |> State.returnM)
    
    let inline opAdd8 v1v2 = 
        let res = v1v2 ||> (+)
        flagAdd8 v1v2 *> (res |> State.returnM)
    
    let execADD instr = 
        match instr.Args with
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM)
             >>= opAdd16
             >>= setReg16 r)
            *> ns
        | [ ArgRegister16 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> Common.signExtend |> State.returnM)
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
        let inc16 get set = 
            Prelude.tuple2 <!> get <*> (1us |> State.returnM)
            >>= opAdd16
            >>= set
        match instr.Args with
        | [ ArgRegister16 AX ] -> 
            let add1 = inc16 (getReg16 AX) (setReg16 AX)
            (getFlag CF <* add1 >>= setFlag CF) *> ns
        | [ ArgRegister8 AL ] -> 
            let add1 = 
                Prelude.tuple2 <!> getReg8 AL <*> (1uy |> State.returnM)
                >>= opAdd8
                >>= setReg8 AL
            (getFlag CF <* add1 >>= setFlag CF) *> ns
        | [ ArgDereference dref ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord16 v)
            (getFlag CF <* (inc16 w16 setMem) >>= setFlag CF) *> ns
        | _ -> nyi instr
    
    let inline coreSUB16 a1a2 = 
        a1a2
        ||> (-)
        |> State.returnM
        <* setSub16Flags a1a2
    
    let inline coreSUB8 a1a2 = 
        a1a2
        ||> (-)
        |> State.returnM
        <* setSub8Flags a1a2
    
    let execSUB instr = 
        match instr.Args with
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            let args = Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM)
            (args
             >>= coreSUB16
             >>= setReg16 r)
            *> ns
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            let args = Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM)
            (args
             >>= coreSUB8
             >>= setReg8 r)
            *> ns
        | _ -> Prelude.undefined
    
    let execCMP instr = 
        match instr.Args with
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM) >>= setSub16Flags) *> ns
        | [ ArgRegister16 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> Common.signExtend |> State.returnM) >>= setSub16Flags) *> ns
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM) >>= setSub8Flags) *> ns
        | [ ArgRegister16 r; ArgDereference dref ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            (Prelude.tuple2 <!> getReg16 r <*> w16 >>= setSub16Flags) *> ns
        | _ -> nyi instr
