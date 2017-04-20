namespace Lib.CPU.Execution
(* 
    NOT AND OR XOR TEST SHL/SAL SHR SAR ROL ROR RCL RCR
*)
module Logic = 
    open FSharpx
    open FSharpx.State
    open Lib
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let setLogicFlags8 (w8 : Word8) = flagSZP8 w8 *> (setFlag Flags.CF false) *> (setFlag Flags.OF false)
    
    let setLogicFlags16 (w16 : Word16) = flagSZP16 w16 *> (setFlag Flags.CF false) *> (setFlag Flags.OF false)
    
    let execNOT instr = 
        match instr.Args with
        // not reg8 3   2   not al
        | [ ArgRegister8 r ] -> (~~~) <!> getReg8 r >>= setReg8 r
        // not [mem8]   16EA    2 to 4  not byte ptr [bx]
        | [ ArgDereference8 dref ] -> (~~~) <!> readMem8 instr dref >>= writeMem8 instr dref
        // not reg16    3   2   not dx
        | [ ArgRegister16 r ] -> (~~~) <!> getReg16 r >>= setReg16 r
        // not [mem16]  24EA    2 to 4  not [WordVar]
        | [ ArgDereference16 dref ] -> (~~~) <!> readMem16 instr dref >>= writeMem16 instr dref
        | _ -> nyi instr
        >>. ns
    
    module SHX =
        let coreSHX8Params = ((0uy, 1uy, 0x80uy, 0xFFuy), 7)
        let coreSHX16Params = ((0us, 1us, 0x8000us, 0xFFFFus), 15)

        let rec loop f acc (x: Word8) =
            if x > 0uy then
                let acc = f acc
                loop f acc (x - 1uy)
            else
                acc

        let inline private coreSHL<'T
                            when 'T : equality
                             and 'T : (static member op_Explicit : ^T -> Word8)
                             and 'T : (static member ( &&& ) : ^T * ^T ->  ^T)
                             and 'T : (static member op_LeftShift : ^T * int32 -> ^T)
                             and 'T : (static member op_RightShift : ^T * int32 -> ^T)>
            (setSZPFlags: 'T -> State<unit, _>) ((v0: 'T, v1: 'T, vMid: 'T, vMax: 'T), bits) (getVal : State<'T, _>) (setVal: 'T -> State<unit, _>) (cnt : Word8) = 
            let doShl = 
                let f = (=<<) (fun s -> setFlag Flags.CF (s &&& vMid <> v0) >>. setVal ((s <<< 1) &&& vMax) >>. getVal)
                loop f getVal cnt
            let setFlags s cf = 
                setFlag Flags.OF (not ((cnt = Word8(v1)) && (cf = ((s >>> bits) = v1)))) >>. setSZPFlags s
            setFlags <!> doShl <*> getFlag Flags.CF >>= id

        let inline private coreSHR<'T
                            when 'T : equality
                             and 'T : (static member op_Explicit : ^T -> Word8)
                             and 'T : (static member ( &&& ) : ^T * ^T ->  ^T)
                             and 'T : (static member op_LeftShift : ^T * int32 -> ^T)
                             and 'T : (static member op_RightShift : ^T * int32 -> ^T)>
            (setSZPFlags: 'T -> State<unit, _>) ((v0: 'T, v1: 'T, vMid: 'T, _: 'T), _) (getVal : State<'T, _>) (setVal: 'T -> State<unit, _>) (cnt : Word8) = 
            let setOF cnt s = (setFlag Flags.OF ((cnt = Word8(v1)) && (s &&& vMid <> v0)))
            let doShr = 
                let f = (=<<) (fun s -> setFlag Flags.CF (s &&& v1 <> v0) >>. setVal (s >>> 1) >>. getVal)
                loop f getVal cnt
            getVal >>= setOF cnt >>. doShr >>. getVal >>= setSZPFlags >>. ns            

        let coreSHL8 = coreSHL flagSZP8 coreSHX8Params
        let coreSHL16 = coreSHL flagSZP16 coreSHX16Params
        let coreSHR8<'T> = coreSHR flagSZP8 coreSHX8Params
        let coreSHR16<'T> = coreSHR flagSZP16 coreSHX16Params

    let execSHL instr = 
        match instr.Args with
        // shl reg8,1  2   2   shl dl,1
        | [ ArgRegister8 r; ArgConstant c ] -> 
            SHX.coreSHL8 (getReg8 r) (setReg8 r) c
        // shl [mem8],1    15EA    2   to 4 shl byte ptr [bxsi],1
        | [ ArgDereference8 dref; ArgConstant c ] -> 
            SHX.coreSHL8 (readMem8 instr dref) (writeMem8 instr dref) c
        // shl reg16,1 2   2   shl cx,1
        | [ ArgRegister16 r; ArgConstant c ] -> 
            SHX.coreSHL16 (getReg16 r) (setReg16 r) c 
        // shl [mem16],1   23EA    2 to 4  shl word ptr [di],1
        | [ ArgDereference16 dref; ArgConstant c ] -> 
            SHX.coreSHL16 (readMem16 instr dref) (writeMem16 instr dref) c
        // shl reg8,cl 8(4*CL) 2   shl al,cl
        | [ ArgRegister8 r; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHL8 (getReg8 r) (setReg8 r))
        // shl [mem8],cl   20EA(4*CL)  2 to 4  shl [ByteVar],cl
        | [ ArgDereference8 dref; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHL8 (readMem8 instr dref) (writeMem8 instr dref))
        // shl reg16,cl    8(4*CL) 2   shl bp,cl
        | [ ArgRegister16 r; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHL16 (getReg16 r) (setReg16 r))
        // shl [mem16],cl  28EA(4*CL)  2 to 4  shl [WordVar1],cl
        | [ ArgDereference16 dref; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHL16 (readMem16 instr dref) (writeMem16 instr dref))
        | _ -> nyi instr
        >>. ns
    
    let execSHR instr = 
        match instr.Args with
        // shr reg8,1  2   2   shr al,1
        | [ ArgRegister8 r; ArgConstant c ] -> 
            SHX.coreSHR8 (getReg8 r) (setReg8 r) c
        // shr [mem8],1    15EA    2 to 4  shr [ByteVar],1
        | [ ArgDereference8 dref; ArgConstant c ] -> 
            SHX.coreSHR8 (readMem8 instr dref) (writeMem8 instr dref) c
        // shr reg16,1 2   2   shr bx,1
        | [ ArgRegister16 r; ArgConstant c ] -> 
            SHX.coreSHR16 (getReg16 r) (setReg16 r) c 
        // shr [mem16],1   23EA    2 to 4  shr word ptr [si],1
        | [ ArgDereference16 dref; ArgConstant c ] -> 
            SHX.coreSHR16 (readMem16 instr dref) (writeMem16 instr dref) c
        // shr reg8,cl 8(4*CL) 2   shr dl,cl
        | [ ArgRegister8 r; ArgRegister8 CL ] -> 
            getReg8 CL >>= SHX.coreSHR8 (getReg8 r) (setReg8 r)
        // shr [mem8],cl   20EA(4*CL)  2 to 4  shr [ByteVarbx],cl
        | [ ArgDereference8 dref; ArgRegister8 CL ] -> 
            getReg8 CL >>= SHX.coreSHR8 (readMem8 instr dref) (writeMem8 instr dref)
        // shr reg16,cl    8(4*CL) 2   shr si,cl
        | [ ArgRegister16 r; ArgRegister8 CL ] -> 
            getReg8 CL >>= SHX.coreSHR16 (getReg16 r) (setReg16 r)
        // shr [mem16],cl  28EA(4*CL)  2 to 4  shr [WordVarsi],cl
        | [ ArgDereference16 dref; ArgRegister8 CL ] -> 
            getReg8 CL >>= SHX.coreSHR16 (readMem16 instr dref) (writeMem16 instr dref)
        | _ -> nyi instr
        >>. ns

    // TODO: Merge TEST with AND with an if condition
    
    let execTEST instr = 
        let inline op8 v1 v2 = 
            let res = v1 &&& v2
            setLogicFlags8 res *> (res |> State.returnM)
    
        let inline op16 v1 v2 = 
            let res = v1 &&& v2
            setLogicFlags16 res *> (res |> State.returnM)

        match instr.Args with
        // test reg8,reg8  3   2   or al,dl
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> 
            op8 <!> getReg8 r1 <*> getReg8 r2 >>= id >>. ns           
        // test [mem8],reg8    16EA    2 to 4  or [ByteVar],ch
        | [ ArgDereference8 dref; ArgRegister8 r ] -> 
            op8 <!> readMem8 instr dref <*> getReg8 r >>= id >>. ns           
        // test reg8,[mem8]    9EA 2 to 4  or bh,[si]
        | [ ArgRegister8 r; ArgDereference8 dref ] -> 
            op8 <!> getReg8 r <*> readMem8 instr dref >>= id >>. ns           
        // test reg16,reg16    3   2   or bp,ax
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            op16 <!> getReg16 r1 <*> getReg16 r2 >>= id >>. ns           
        // test [mem16],reg16  24EA    2 to 4  or [bpsi],cx
        | [ ArgDereference16 dref; ArgRegister16 r ] -> 
            op16 <!> readMem16 instr dref <*> getReg16 r >>= id >>. ns           
        // test reg16,[mem16]  13EA    2 to 4  or ax,[bx]
        | [ ArgRegister16 r; ArgDereference16 dref ] -> 
            op16 <!> getReg16 r <*> readMem16 instr dref >>= id >>. ns           
        // test reg8,immed8    4   3   or cl,03h
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            op8 <!> getReg8 r <*> (c |> State.returnM) >>= id >>. ns           
        // test [mem8],immed8  17EA    3 to 5  or [ByteVar1],29h
        | [ ArgDereference8 dref; ArgImmediate(W8 c) ] -> 
            op8 <!> readMem8 instr dref <*> (c |> State.returnM) >>= id >>. ns           
        // test reg16,immed16  4   4   or ax,01fffh
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            op16 <!> getReg16 r <*> (c |> State.returnM) >>= id >>. ns           
        // test [mem16],immed16    25EA    4 to 6  or [WordVar],7fffh
        | [ ArgDereference16 dref; ArgImmediate(W16 c) ] -> 
            op16 <!> readMem16 instr dref <*> (c |> State.returnM) >>= id >>. ns           
        // test al,immed8  4   2   or al,0c0h
        // test ax,immed16 4   3   or ax,01ffh
        | _ -> nyi instr

    let execLogicOp op8 op16 instr = 
        let inline op8 v1 v2 = 
            let res = op8 v1 v2
            setLogicFlags8 res *> (res |> State.returnM)
    
        let inline op16 v1 v2 = 
            let res = op16 v1 v2
            setLogicFlags16 res *> (res |> State.returnM)

        match instr.Args with
        // or/and/xor reg8,reg8  3   2   or al,dl
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> 
            op8 <!> getReg8 r1 <*> getReg8 r2 >>= id 
            >>= setReg8 r1           
        // or/and/xor [mem8],reg8    16EA    2 to 4  or [ByteVar],ch
        | [ ArgDereference8 dref; ArgRegister8 r ] -> 
            op8 <!> readMem8 instr dref <*> getReg8 r >>= id
            >>= writeMem8 instr dref           
        // or/and/xor reg8,[mem8]    9EA 2 to 4  or bh,[si]
        | [ ArgRegister8 r; ArgDereference8 dref ] -> 
            op8 <!> getReg8 r <*> readMem8 instr dref >>= id
            >>= setReg8 r           
        // or/and/xor reg16,reg16    3   2   or bp,ax
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            op16 <!> getReg16 r1 <*> getReg16 r2 >>= id
            >>= setReg16 r1           
        // or/and/xor [mem16],reg16  24EA    2 to 4  or [bpsi],cx
        | [ ArgDereference16 dref; ArgRegister16 r ] -> 
            op16 <!> readMem16 instr dref <*> getReg16 r >>= id
            >>= writeMem16 instr dref           
        // or/and/xor reg16,[mem16]  13EA    2 to 4  or ax,[bx]
        | [ ArgRegister16 r; ArgDereference16 dref ] -> 
            op16 <!> getReg16 r <*> readMem16 instr dref >>= id
            >>= setReg16 r           
        // or/and/xor reg8,immed8    4   3   or cl,03h
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            op8 <!> getReg8 r <*> (c |> State.returnM) >>= id 
            >>= setReg8 r           
        // or/and/xor [mem8],immed8  17EA    3 to 5  or [ByteVar1],29h
        | [ ArgDereference8 dref; ArgImmediate(W8 c) ] -> 
            op8 <!> readMem8 instr dref <*> (c |> State.returnM) >>= id
            >>= writeMem8 instr dref           
        // or/and/xor reg16,sextimmed    4   3   or ax,01fh
        | [ ArgRegister16 r; ArgImmediate(W8 c) ] -> 
            op16 <!> getReg16 r <*> (c |> Common.signExtend |> State.returnM) >>= id
            >>= setReg16 r           
        // or/and/xor reg16,immed16  4   4   or ax,01fffh
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            op16 <!> getReg16 r <*> (c |> State.returnM) >>= id 
            >>= setReg16 r           
        // or/and/xor [mem16],sextimmed  25EA    3 to 5  or [WordVar],7fh
        | [ ArgDereference16 dref; ArgImmediate(W8 c) ] -> 
            op16 <!> readMem16 instr dref <*> (c |> Common.signExtend |> State.returnM) >>= id
            >>= writeMem16 instr dref           
        // or/and/xor [mem16],immed16    25EA    4 to 6  or [WordVar],7fffh
        | [ ArgDereference16 dref; ArgImmediate(W16 c) ] -> 
            op16 <!> readMem16 instr dref <*> (c |> State.returnM) >>= id
            >>= writeMem16 instr dref           
        // or/and/xor al,immed8  4   2   or al,0c0h
        // or/and/xor ax,immed16 4   3   or ax,01ffh
        | _ -> nyi instr
        >>. ns

    let execAND instr = execLogicOp (&&&) (&&&) instr

    let execOR instr = execLogicOp (|||) (|||) instr

    let execXOR instr = execLogicOp (^^^) (^^^) instr
