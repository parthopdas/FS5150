namespace Lib.CPU.Execution
(* 
    ADD ADC INC AAA DA SUB SBB DEC NEG CMP AAS DA MUL IMUL AA DIV IDIV AAD CBW CWD
*)
module Arithmetic = 
    open YaFunTK
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open Lib
    
    module ADD =
        let inline private setAddFlags<'T, 'TUp
                            when 'TUp : equality
                             and 'TUp : (static member ( ^^^ ) :  ^TUp *  ^TUp ->  ^TUp)
                             and 'TUp : (static member ( &&& ) :  ^TUp *  ^TUp ->  ^TUp)>
                (op : 'TUp -> 'TUp -> 'TUp, fdn : 'TUp -> 'T, fup : 'T -> 'TUp) setSZPFlags (vff00, v0, vMid, v10) (v1 : 'T, v2 : 'T) = 
            let dst = op (fup(v1)) (fup(v2))
            (setSZPFlags (fdn(dst))) 
            *> (setFlag Flags.CF (dst &&& vff00 <> v0)) 
            *> (setFlag Flags.OF (((dst ^^^ fup(v1)) &&& (dst ^^^ fup(v2)) &&& vMid) = vMid)) 
            *> (setFlag Flags.AF (((fup(v1) ^^^ fup(v2) ^^^ dst) &&& v10) = v10))

        let private add8ValParams = (0xFF00us, 0us, 0x80us, 0x10us)
        let private add8FunParams = ((+), uint8, uint16)
        let private add16ValParams = (0xFFFF0000ul, 0ul, 0x8000ul, 0x10ul)
        let private add16FunParams = ((+), uint16, uint32)
    
        let setAdd8Flags = setAddFlags add8FunParams flagSZP8 add8ValParams

        let setAdd16Flags = setAddFlags add16FunParams flagSZP16 add16ValParams

        let inline opAdd16 v1v2 = 
            let res = v1v2 ||> (+)
            setAdd16Flags v1v2 *> (res |> State.returnM)
    
        let inline opAdd8 v1v2 = 
            let res = v1v2 ||> (+)
            setAdd8Flags v1v2 *> (res |> State.returnM)
    
        let inline private incCore<'T>
            add (v1 : 'T) (get : State<'T, _>) (set : 'T -> State<unit, _>) =
            let add1 = Prelude.tuple2 <!> get <*> (v1 |> State.returnM) >>= add >>= set
            (getFlag Flags.CF <* add1 >>= setFlag Flags.CF)
        let inc8 = incCore opAdd8 1uy
        let inc16 = incCore opAdd16 1us

    // TODO: PERF: Prelude.Tuple is unnecessary

    let execADD instr = 
        match instr.Args with
        // add reg8,reg8    3    2    add ah,al
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> 
            (Prelude.tuple2 <!> getReg8 r1 <*> getReg8 r2
                 >>= ADD.opAdd8
                 >>= setReg8 r1)
            *> ns
        // add [mem8],reg8    16EA    2 to 4    add [bx1],dh
        | [ ArgDereference8 dref; ArgRegister8 r ] -> 
            (Prelude.tuple2 <!> readMem8 instr dref <*> getReg8 r
                 >>= ADD.opAdd8
                 >>= writeMem8 instr dref)
            *> ns
        // add reg8,[mem8]    9EA    2 to 4    add ch,[bx]
        | [ ArgRegister8 r; ArgDereference8 dref ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> readMem8 instr dref
                 >>= ADD.opAdd8
                 >>= setReg8 r)
            *> ns
        // add reg16,reg16    3    2    add dx,ax
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> getReg16 r2
                 >>= ADD.opAdd16
                 >>= setReg16 r1)
            *> ns
        // add [mem16],reg16    24EA    2 to 4    add [bp5],ax
        | [ ArgDereference16 dref; ArgRegister16 r ] -> 
            (Prelude.tuple2 <!> readMem16 instr dref <*> getReg16 r
                 >>= ADD.opAdd16
                 >>= writeMem16 instr dref)
            *> ns
        // add reg16,[mem16]    13EA    2 to 4    add ax,[Basedi]
        | [ ArgRegister16 r; ArgDereference16 dref ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> readMem16 instr dref
                 >>= ADD.opAdd16
                 >>= setReg16 r)
            *> ns
        // add reg8,immed8    4    3    add dl,16
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM)
                 >>= ADD.opAdd8
                 >>= setReg8 r)
            *> ns
        // add [mem8],immed8    17EA    3 to 5    add byte ptr [si6],0c3h
        | [ ArgDereference8 dref; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> readMem8 instr dref <*> (c |> State.returnM)
                 >>= ADD.opAdd8
                 >>= writeMem8 instr dref)
            *> ns
        // add reg16,sextimmed    4    3    add si,0ff80h
        | [ ArgRegister16 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> Common.signExtend |> State.returnM)
                 >>= ADD.opAdd16
                 >>= setReg16 r)
            *> ns
        // add reg16,immed16    4    4    add si,8000h
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM)
                 >>= ADD.opAdd16
                 >>= setReg16 r)
            *> ns
        // add [mem16],sextimmed    25EA    3 to 5    add [WordVar],3
        | [ ArgDereference16 dref; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> readMem16 instr dref <*> (c |> Common.signExtend |> State.returnM)
                 >>= ADD.opAdd16
                 >>= writeMem16 instr dref)
            *> ns
        // add [mem16],immed16    25EA    4 to 6    add [WordVar],300h
        | [ ArgDereference16 dref; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> readMem16 instr dref <*> (c |> State.returnM)
                 >>= ADD.opAdd16
                 >>= writeMem16 instr dref)
            *> ns
        // add al,immed8    4    2    add al,1
        // add ax,immed16    4    3    add ax,2
        | _ -> nyi instr

    let execINC instr = 
        match instr.Args with
        // inc reg8 3   2   inc ah
        | [ ArgRegister8 r ] -> 
            (ADD.inc8 (getReg8 r) (setReg8 r))*> ns
        // inc [mem8]   15EA    2 to 4  inc byte ptr [bx]
        | [ ArgDereference8 dref ] -> 
            (ADD.inc8 (readMem8 instr dref) (writeMem8 instr dref))*> ns
        // inc reg16    2   1   inc si
        | [ ArgRegister16 r ] -> 
            (ADD.inc16 (getReg16 r) (setReg16 r))*> ns
        // inc [mem16]  23EA    2 to 4  inc [WordVar]
        | [ ArgDereference16 dref ] -> 
            (ADD.inc16 (readMem16 instr dref) (writeMem16 instr dref))*> ns
        | _ -> nyi instr
    
    module SUB =
        let inline coreSUB16 a1a2 = a1a2 ||> (-) |> State.returnM <* setSub16Flags a1a2
    
        let inline coreSUB8 a1a2 = a1a2 ||> (-) |> State.returnM <* setSub8Flags a1a2
    
        let inline private decCore<'T>
            sub (v1 : 'T) (get : State<'T, _>) (set : 'T -> State<unit, _>) =
            let sub1 = Prelude.tuple2 <!> get <*> (v1 |> State.returnM) >>= sub >>= set
            (getFlag Flags.CF <* sub1 >>= setFlag Flags.CF)
        let dec8 = decCore coreSUB8 1uy
        let dec16 = decCore coreSUB16 1us

    let execSUBCore writeSrc instr = 
        let postOp x =
            if writeSrc then
                x
            else
                ignore >> State.returnM

        match instr.Args with
        // sub reg8,reg8    3   2   sub al,dl
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> 
            (Prelude.tuple2 <!> getReg8 r1 <*> getReg8 r2 
                >>= SUB.coreSUB8 
                >>= postOp(setReg8 r1))
            *> ns
        // sub [mem8],reg8  16EA    2 to 4  sub [ByteVar],ah
        | [ ArgDereference8 dref; ArgRegister8 r ] -> 
            (Prelude.tuple2 <!> readMem8 instr dref <*> getReg8 r 
                >>= SUB.coreSUB8 
                >>= postOp(writeMem8 instr dref))
            *> ns
        // sub reg8,[mem8]  9EA 2 to 4  sub dl,[si1]
        | [ ArgRegister8 r; ArgDereference8 dref ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> readMem8 instr dref 
                >>= SUB.coreSUB8 
                >>= postOp(setReg8 r))
            *> ns
        // sub reg16,reg16  3   2   sub ax,dx
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> getReg16 r2 
                >>= SUB.coreSUB16 
                >>= postOp(setReg16 r1))
            *> ns
        // sub [mem16],reg16    24EA    2 to 4  sub [WordVar],ax
        | [ ArgDereference16 dref; ArgRegister16 r ] -> 
            (Prelude.tuple2 <!> readMem16 instr dref <*> getReg16 r 
                >>= SUB.coreSUB16 
                >>= postOp(writeMem16 instr dref))
            *> ns
        // sub reg16,[mem16]    13EA    2 to 4  sub cx,[dibp]
        | [ ArgRegister16 r; ArgDereference16 dref ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> readMem16 instr dref 
                >>= SUB.coreSUB16 
                >>= postOp(setReg16 r))
            *> ns
        // sub reg8,immed8  4   3   sub dl,10h
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg8 r <*> (c |> State.returnM) 
                >>= SUB.coreSUB8 
                >>= postOp(setReg8 r))
            *> ns
        // sub [mem8],immed8    17EA    3 to 5  sub [ByteVar],01h
        | [ ArgDereference8 dref; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> readMem8 instr dref <*> (c |> State.returnM) 
                >>= SUB.coreSUB8 
                >>= postOp(writeMem8 instr dref))
            *> ns
        // sub reg16,sextimmed  4   3   sub dx,1
        | [ ArgRegister16 r; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> Common.signExtend |> State.returnM) 
                >>= SUB.coreSUB16 
                >>= postOp(setReg16 r))
            *> ns
        // sub reg16,immed16    4   4   sub dx,80h
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> getReg16 r <*> (c |> State.returnM) 
                >>= SUB.coreSUB16 
                >>= postOp(setReg16 r))
            *> ns
        // sub [mem16],sextimmed    25EA    3 to 5  sub word ptr [bp],10h
        | [ ArgDereference16 dref; ArgImmediate(W8 c) ] -> 
            (Prelude.tuple2 <!> readMem16 instr dref <*> (c |> Common.signExtend |> State.returnM) 
                >>= SUB.coreSUB16 
                >>= postOp(writeMem16 instr dref))
            *> ns
        // sub [mem16],immed16  25EA    4 to 6  sub word ptr [bp],100h
        | [ ArgDereference16 dref; ArgImmediate(W16 c) ] -> 
            (Prelude.tuple2 <!> readMem16 instr dref <*> (c |> State.returnM) 
                >>= SUB.coreSUB16 
                >>= postOp(writeMem16 instr dref))
            *> ns
        // sub al,immed8    4   2   sub al,20h
        // sub ax,immed16   4   3   sub ax,100h
        | _ -> nyi

    let execSUB instr = execSUBCore true instr
    let execCMP instr = execSUBCore false instr

    let execDEC instr = 
        match instr.Args with
        // dec reg8 3   2   dec ah
        | [ ArgRegister8 r ] -> 
            (SUB.dec8 (getReg8 r) (setReg8 r)) *> ns
        // dec [mem8]   15EA    2 to 4  dec byte ptr [bx]
        | [ ArgDereference8 dref ] -> 
            (SUB.dec8 (readMem8 instr dref) (writeMem8 instr dref))*> ns
        // dec reg16    2   1   dec si
        | [ ArgRegister16 r ] -> 
            (SUB.dec16 (getReg16 r) (setReg16 r))*> ns
        // dec [mem16]  23EA    2 to 4  dec [WordVar]
        | [ ArgDereference16 dref ] -> 
            (SUB.dec16 (readMem16 instr dref) (writeMem16 instr dref))*> ns
        | _ -> nyi instr