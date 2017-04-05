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
        *> (setFlag Flags.CF (dst &&& 0xFFFF0000ul <> 0ul)) 
        *> (setFlag Flags.OF (((dst ^^^ (uint32) v1) &&& (dst ^^^ (uint32) v2) &&& 0x8000ul) = 0x8000ul)) 
        *> (setFlag Flags.AF ((((uint32) v1 ^^^ (uint32) v2 ^^^ dst) &&& 0x10ul) = 0x10ul))
    
    let flagAdd8 (v1 : Word8, v2 : Word8) = 
        let dst = (uint16) v1 + (uint16) v2
        (flagSZP8 ((uint8) dst)) 
        *> (setFlag Flags.CF (dst &&& 0xFF00us <> 0us)) 
        *> (setFlag Flags.OF (((dst ^^^ (uint16) v1) &&& (dst ^^^ (uint16) v2) &&& 0x80us) = 0x80us)) 
        *> (setFlag Flags.AF ((((uint16) v1 ^^^ (uint16) v2 ^^^ dst) &&& 0x10us) = 0x10us))
    
    let inline opAdd16 v1v2 = 
        let res = v1v2 ||> (+)
        flagAdd16 v1v2 *> (res |> State.returnM)
    
    let inline opAdd8 v1v2 = 
        let res = v1v2 ||> (+)
        flagAdd8 v1v2 *> (res |> State.returnM)
    
    // TODO: PERF: Prelude.Tuple is unnecessary

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
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> 
            (Prelude.tuple2 <!> getReg8 r1 <*> getReg8 r2
             >>= opAdd8
             >>= setReg8 r1)
            *> ns
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> getReg16 r2
             >>= opAdd16
             >>= setReg16 r1)
            *> ns
        | [ ArgDereference dref; ArgRegister8 r ] -> 
            let w8 = addressFromDref instr dref >>= readWord8
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord8 v)
            (Prelude.tuple2 <!> w8 <*> getReg8 r
             >>= opAdd8
             >>= setMem)
            *> ns
        | [ ArgRegister8 r; ArgDereference dref ] -> 
            let w8 = addressFromDref instr dref >>= readWord8
            (Prelude.tuple2 <!> getReg8 r <*> w8
             >>= opAdd8
             >>= setReg8 r)
            *> ns
        | [ ArgDereference dref; ArgRegister16 r ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord16 v)
            (Prelude.tuple2 <!> w16 <*> getReg16 r
             >>= opAdd16
             >>= setMem)
            *> ns
        | [ ArgRegister16 r; ArgDereference dref ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            (Prelude.tuple2 <!> getReg16 r <*> w16
             >>= opAdd16
             >>= setReg16 r)
            *> ns
        | [ ArgDereference dref; ArgImmediate(W8 c) ] -> 
            let w8 = addressFromDref instr dref >>= readWord8
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord8 v)
            (Prelude.tuple2 <!> w8 <*> (c |> State.returnM)
             >>= opAdd8
             >>= setMem)
            *> ns
        | [ ArgDereference dref; ArgImmediate(W16 c) ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord16 v)
            (Prelude.tuple2 <!> w16 <*> (c |> State.returnM)
             >>= opAdd16
             >>= setMem)
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
            (getFlag Flags.CF <* add1 >>= setFlag Flags.CF) *> ns
        | [ ArgRegister8 AL ] -> 
            let add1 = 
                Prelude.tuple2 <!> getReg8 AL <*> (1uy |> State.returnM)
                >>= opAdd8
                >>= setReg8 AL
            (getFlag Flags.CF <* add1 >>= setFlag Flags.CF) *> ns
        | [ ArgDereference dref ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord16 v)
            (getFlag Flags.CF <* (inc16 w16 setMem) >>= setFlag Flags.CF) *> ns
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
        | [ ArgDereference dref; ArgImmediate(W8 c) ] -> 
            let w8 = addressFromDref instr dref >>= readWord8
            (Prelude.tuple2 <!> w8 <*> (c |> State.returnM) >>= setSub8Flags) *> ns
        | [ ArgDereference dref; ArgImmediate(W16 c) ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            (Prelude.tuple2 <!> w16 <*> (c |> State.returnM) >>= setSub16Flags) *> ns
        | _ -> nyi instr
