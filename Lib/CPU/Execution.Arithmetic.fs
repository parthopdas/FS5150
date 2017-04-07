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
        (flagSZP16 ((Word16) dst)) 
        *> (setFlag Flags.CF (dst &&& 0xFFFF0000ul <> 0ul)) 
        *> (setFlag Flags.OF (((dst ^^^ (uint32) v1) &&& (dst ^^^ (uint32) v2) &&& 0x8000ul) = 0x8000ul)) 
        *> (setFlag Flags.AF ((((uint32) v1 ^^^ (uint32) v2 ^^^ dst) &&& 0x10ul) = 0x10ul))
    
    let flagAdd8 (v1 : Word8, v2 : Word8) = 
        let dst = (uint16) v1 + (uint16) v2
        (flagSZP8 ((Word8) dst)) 
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

    // TODO: Implement: add [mem16],sextimmed    25EA    3 to 5    add [WordVar],3
    let execADD instr = 
        match instr.Args with
        // add reg8,reg8    3    2    add ah,al
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> 
            (Prelude.tuple2 <!> getReg8 r1 <*> getReg8 r2
             >>= opAdd8
             >>= setReg8 r1)
            *> ns
        // add [mem8],reg8    16EA    2 to 4    add [bx1],dh
        | [ ArgDereference dref; ArgRegister8 r ] -> 
            let w8 = addressFromDref instr dref >>= readWord8
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord8 v)
            (Prelude.tuple2 <!> w8 <*> getReg8 r
             >>= opAdd8
             >>= setMem)
            *> ns
        // add reg8,[mem8]    9EA    2 to 4    add ch,[bx]
        | [ ArgRegister8 r; ArgDereference dref ] -> 
            let w8 = addressFromDref instr dref >>= readWord8
            (Prelude.tuple2 <!> getReg8 r <*> w8
             >>= opAdd8
             >>= setReg8 r)
            *> ns
        // add reg16,reg16    3    2    add dx,ax
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> getReg16 r2
             >>= opAdd16
             >>= setReg16 r1)
            *> ns
        // add [mem16],reg16    24EA    2 to 4    add [bp5],ax
        | [ ArgDereference dref; ArgRegister16 r ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord16 v)
            (Prelude.tuple2 <!> w16 <*> getReg16 r
             >>= opAdd16
             >>= setMem)
            *> ns
        // add reg16,[mem16]    13EA    2 to 4    add ax,[Basedi]
        | [ ArgRegister16 r; ArgDereference dref ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            (Prelude.tuple2 <!> getReg16 r <*> w16
             >>= opAdd16
             >>= setReg16 r)
            *> ns
        // add reg8,immed8    4    3    add dl,16
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM)
             >>= opAdd8
             >>= setReg8 r)
            *> ns
        // add [mem8],immed8    17EA    3 to 5    add byte ptr [si6],0c3h
        | [ ArgDereference dref; ArgImmediate(W8 c) ] -> 
            let w8 = addressFromDref instr dref >>= readWord8
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord8 v)
            (Prelude.tuple2 <!> w8 <*> (c |> State.returnM)
             >>= opAdd8
             >>= setMem)
            *> ns
        // add reg16,sextimmed    4    3    add si,0ff80h
        | [ ArgRegister16 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> Common.signExtend |> State.returnM)
             >>= opAdd16
             >>= setReg16 r)
            *> ns
        // add reg16,immed16    4    4    add si,8000h
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM)
             >>= opAdd16
             >>= setReg16 r)
            *> ns
        // add [mem16],sextimmed    25EA    3 to 5    add [WordVar],3
        // TODO: Why didn't the test case hit this?
        // add [mem16],immed16    25EA    4 to 6    add [WordVar],300h
        | [ ArgDereference dref; ArgImmediate(W16 c) ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord16 v)
            (Prelude.tuple2 <!> w16 <*> (c |> State.returnM)
             >>= opAdd16
             >>= setMem)
            *> ns
        // add al,immed8    4    2    add al,1
        // add ax,immed16    4    3    add ax,2
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
        // cmp reg8,reg8    3    2    cmp ah,al
        // cmp [mem8],reg8    9EA    2 to 4    cmp [si],cl
        // cmp reg8,[mem8]    9EA    2 to 4    cmp ah,[bx]
        // cmp reg16,reg16    3    2    cmp dx,ax
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> getReg16 r2 >>= setSub16Flags) *> ns
        // cmp [mem16],reg16    13EA    2 to 4    cmp [bxdiRecPtr],bx
        // cmp reg16,[mem16]    13EA    2 to 4    cmp bp,[bx1]
        | [ ArgRegister16 r; ArgDereference dref ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            (Prelude.tuple2 <!> getReg16 r <*> w16 >>= setSub16Flags) *> ns
        // cmp reg8,immed8    4    3    cmp ah,9
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM) >>= setSub8Flags) *> ns
        // cmp [mem8],immed8    10EA    3 to 5    cmp [ByteVar],39h
        | [ ArgDereference dref; ArgImmediate(W8 c) ] -> 
            let w8 = addressFromDref instr dref >>= readWord8
            (Prelude.tuple2 <!> w8 <*> (c |> State.returnM) >>= setSub8Flags) *> ns
        // cmp reg16,sextimmed    4    3    cmp dx,8
        | [ ArgRegister16 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> Common.signExtend |> State.returnM) >>= setSub16Flags) *> ns
        // cmp reg16,immed16    4    4    cmp sp,999h
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM) >>= setSub16Flags) *> ns
        // cmp [mem16],sextimmed    14EA    3 to 5    cmp [WordVar],12
        // cmp [mem16],immed16    14EA    4 to 6    cmp [WordVar],92h
        | [ ArgDereference dref; ArgImmediate(W16 c) ] -> 
            let w16 = addressFromDref instr dref >>= readWord16
            (Prelude.tuple2 <!> w16 <*> (c |> State.returnM) >>= setSub16Flags) *> ns
        // cmp al,immed8    4    2    cmp al,22
        // cmp ax,immed16    4    3    cmp ax,722
        | _ -> nyi instr
