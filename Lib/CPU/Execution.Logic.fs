namespace Lib.CPU.Execution

module Logic = 
    open YaFunTK
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let flagLog8 (w8 : Word8) = flagSZP8 w8 *> (setFlag Flags.CF false) *> (setFlag Flags.OF false)
    
    let flagLog16 (w16 : Word16) = flagSZP16 w16 *> (setFlag Flags.CF false) *> (setFlag Flags.OF false)
    
    let execNOT instr = 
        match instr.Args with
        | [ ArgRegister8 r ] -> ((~~~) <!> getReg8 r >>= setReg8 r) *> ns
        | [ ArgRegister16 r ] -> ((~~~) <!> getReg16 r >>= setReg16 r) *> ns
        | _ -> nyi instr
    
    module SHX =
        let coreSHX8Params = (0uy, 1uy, 0x80uy, 0xFFuy, 7)
        let coreSHX16Params = (0us, 1us, 0x8000us, 0xFFFFus, 15)

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
            (setSZPFlags: 'T -> State<unit, _>) (v0: 'T, v1: 'T, vMid: 'T, vMax: 'T, bits) (getVal : State<'T, _>) (setVal: 'T -> State<unit, _>) (cnt : Word8) = 
            let doShl = 
                let f = (=<<) (fun s -> setFlag Flags.CF (s &&& vMid <> v0) *> setVal ((s <<< 1) &&& vMax) *> getVal)
                loop f getVal cnt
            ((Prelude.tuple2 <!> doShl <*> getFlag Flags.CF) 
                >>= (fun (s, cf) -> (setFlag Flags.OF (not ((cnt = Word8(v1)) && (cf = ((s >>> bits) = v1))))) *> setSZPFlags s))

        let inline private coreSHR<'T
                            when 'T : equality
                             and 'T : (static member op_Explicit : ^T -> Word8)
                             and 'T : (static member ( &&& ) : ^T * ^T ->  ^T)
                             and 'T : (static member op_LeftShift : ^T * int32 -> ^T)
                             and 'T : (static member op_RightShift : ^T * int32 -> ^T)>
            (setSZPFlags: 'T -> State<unit, _>) (v0: 'T, v1: 'T, vMid: 'T, _: 'T, _) (getVal : State<'T, _>) (setVal: 'T -> State<unit, _>) (cnt : Word8) = 
            let setOF cnt s = (setFlag Flags.OF ((cnt = Word8(v1)) && (s &&& vMid <> v0)))
            let doShr = 
                let f = (=<<) (fun s -> setFlag Flags.CF (s &&& v1 <> v0) *> setVal (s >>> 1) *> getVal)
                loop f getVal cnt

            (getVal >>= setOF cnt) *> doShr *> (getVal >>= setSZPFlags) *> ns            

        let coreSHL8 = coreSHL flagSZP8 coreSHX8Params
        let coreSHL16 = coreSHL flagSZP16 coreSHX16Params
        let coreSHR8<'T> = coreSHR flagSZP8 coreSHX8Params
        let coreSHR16<'T> = coreSHR flagSZP16 coreSHX16Params

    // TODO 
    // - dreference needs to support differentiating between 8 and 16 bits
    //   - 8 bit and 16 bit dereferences in ADD and MOV
    let execSHL instr = 
        match instr.Args with
        // shl reg8,1  2   2   shl dl,1
        | [ ArgRegister8 r; ArgConstant c ] -> 
            SHX.coreSHL8 (getReg8 r) (setReg8 r) c *> ns
        // shl [mem8],1    15EA    2   to 4 shl byte ptr [bxsi],1
        | [ ArgDereference dref; ArgConstant c ] -> 
            SHX.coreSHL8 (readMem8 instr dref) (writeMem8 instr dref) c *> ns
        // shl reg16,1 2   2   shl cx,1
        | [ ArgRegister16 r; ArgConstant c ] -> 
            SHX.coreSHL16 (getReg16 r) (setReg16 r) c *> ns 
        // shl [mem16],1   23EA    2 to 4  shl word ptr [di],1
        // ???
        // shl reg8,cl 8(4*CL) 2   shl al,cl
        | [ ArgRegister8 r; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHL8 (getReg8 r) (setReg8 r)) *> ns
        // shl [mem8],cl   20EA(4*CL)  2 to 4  shl [ByteVar],cl
        | [ ArgDereference dref; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHL8 (readMem8 instr dref) (writeMem8 instr dref)) *> ns
        // shl reg16,cl    8(4*CL) 2   shl bp,cl
        | [ ArgRegister16 r; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHL16 (getReg16 r) (setReg16 r)) *> ns
        // shl [mem16],cl  28EA(4*CL)  2 to 4  shl [WordVar1],cl
        // ???
        | _ -> nyi instr
    
    let execSHR instr = 
        match instr.Args with
        // shr reg8,1  2   2   shr al,1
        | [ ArgRegister8 r; ArgConstant c ] -> 
            SHX.coreSHR8 (getReg8 r) (setReg8 r) c *> ns
        // shr [mem8],1    15EA    2 to 4  shr [ByteVar],1
        | [ ArgDereference dref; ArgConstant c ] -> 
            SHX.coreSHR8 (readMem8 instr dref) (writeMem8 instr dref) c *> ns
        // shr reg16,1 2   2   shr bx,1
        | [ ArgRegister16 r; ArgConstant c ] -> 
            SHX.coreSHR16 (getReg16 r) (setReg16 r) c *> ns 
        // shr [mem16],1   23EA    2 to 4  shr word ptr [si],1
        // ???
        // shr reg8,cl 8(4*CL) 2   shr dl,cl
        | [ ArgRegister8 r; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHR8 (getReg8 r) (setReg8 r)) *> ns
        // shr [mem8],cl   20EA(4*CL)  2 to 4  shr [ByteVarbx],cl
        // ???
        // shr reg16,cl    8(4*CL) 2   shr si,cl
        | [ ArgRegister16 r; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHR16 (getReg16 r) (setReg16 r)) *> ns
        // shr [mem16],cl  28EA(4*CL)  2 to 4  shr [WordVarsi],cl
        | [ ArgDereference dref; ArgRegister8 CL ] -> 
            (getReg8 CL >>= SHX.coreSHR16 (readMem16 instr dref) (writeMem16 instr dref)) *> ns
        | _ -> nyi instr
    
    let execXOR instr = 
        let opXor8 v1v2 = 
            let res = v1v2 ||> (^^^)
            flagLog8 res *> (res |> State.returnM)
    
        let opXor16 v1v2 = 
            let res = v1v2 ||> (^^^)
            flagLog16 res *> (res |> State.returnM)
    
        match instr.Args with
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> getReg16 r2
             >>= opXor16
             >>= setReg16 r1)
            *> ns
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> 
            (Prelude.tuple2 <!> getReg8 r1 <*> getReg8 r2
             >>= opXor8
             >>= setReg8 r1)
            *> ns
        | [ ArgRegister16 r1; ArgImmediate(W16 w16) ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> (w16 |> State.returnM)
             >>= opXor16
             >>= setReg16 r1)
            *> ns
        | _ -> nyi instr

    let execTEST instr = 
        let opAnd8 v1v2 = 
            let res = v1v2 ||> (&&&)
            flagLog8 res *> (res |> State.returnM)
    
        let opAnd16 v1v2 = 
            let res = v1v2 ||> (&&&)
            flagLog16 res *> (res |> State.returnM)
    
        match instr.Args with
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM)
             >>= opAnd8)
            *> ns
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM)
             >>= opAnd16)
            *> ns
        | _ -> nyi instr
    
