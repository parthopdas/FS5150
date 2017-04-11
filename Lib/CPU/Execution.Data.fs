namespace Lib.CPU.Execution

(* 
    MOV PUSH POP XCHG XLAT/XLATB IN OUT LEA LDS LE LAHF SAHF PUSHF POPF 
*)

module Data = 
    open YaFunTK
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let execMOV instr = 
        match instr.Args with
        // mov reg8,reg8    2    2    mov ch,al
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> ((getReg8 r2) >>= (setReg8 r1)) *> ns
        // mov [mem8],reg8    9EA    2 to 4    mov [bx10h],dh
        | [ ArgDereference8 dref; ArgRegister8 r ] -> 
            (getReg8 r >>= writeMem8 instr dref) *> ns
        // mov reg8,[mem8]    8EA    2 to 4    mov bl,[si]
        | [ ArgRegister8 r; ArgDereference8 dref ] -> 
            (readMem8 instr dref >>= setReg8 r) *> ns
        // mov reg16,reg16    2    2    mov ax,dx
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> ((getReg16 r2) >>= (setReg16 r1)) *> ns
        // mov [mem16],reg16    13EA    2 to 4    mov [WordVar],cx
        | [ ArgDereference16 dref; ArgRegister16 r ] -> 
            (getReg16 r >>= writeMem16 instr dref) *> ns
        // mov reg16,[mem16]    12EA    2 to 4    mov bx,[Tablebx]
        | [ ArgRegister16 r; ArgDereference16 dref ] -> 
            (readMem16 instr dref >>= setReg16 r) *> ns
        // mov reg8,immed8    4    2    mov dl,1
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> (setReg8 r c) *> ns
        // mov [mem8],immed8    10EA    3 to 5    mov [ByteVar],1
        | [ ArgDereference8 dref; ArgImmediate(W8 c) ] -> (writeMem8 instr dref c) *> ns
        // mov reg16,immed16    4    3    mov ax,88h
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> (setReg16 r c) *> ns
        // mov [mem16],immed16    14EA    4 to 6    mov [WordVar],1000h
        | [ ArgDereference16 dref; ArgImmediate(W16 c) ] -> (writeMem16 instr dref c) *> ns
        // mov al,[mem8] (direct)    10    3    mov al,[Flag]
        // mov [mem8],al (direct)    10    3    mov [ByteVar],al
        // mov ax,[mem16] (direct)    14    3    mov ax,[WordVar]
        // mov [mem16],ax (direct)    14    3    mov [Count],ax
        // mov segreg,reg16    2    2    mov es,ax
        | [ ArgRegisterSeg r1; ArgRegister16 r2 ] -> ((getReg16 r2) >>= (setRegSeg r1)) *> ns
        // mov segreg,[mem16]    12EA    2 to 4    mov ds,[DataPtrsbx]
        | [ ArgRegisterSeg r; ArgDereference16 dref ] -> 
            (readMem16 instr dref >>= setRegSeg r) *> ns
        // mov reg16,segreg    2    2    mov dx,ds
        | [ ArgRegister16 r1; ArgRegisterSeg r2 ] -> ((getRegSeg r2) >>= (setReg16 r1)) *> ns
        // mov [mem16],segreg    13EA    2 to 4    mov [StackSeg],ss
        | [ ArgDereference16 dref; ArgRegisterSeg r ] -> 
            (getRegSeg r >>= writeMem16 instr dref) *> ns
        | _ -> nyi instr
    
    let execOUT instr = 
        match instr.Args with
        | [ ArgImmediate(W8 c); ArgRegister8 r ] -> (getReg8 r >>= portWrite !<>c) *> ns
        | [ ArgRegister16 pno; ArgRegister8 v ] -> (getReg16 pno >>= (fun pno -> getReg8 v >>= portWrite pno)) *> ns
        | _ -> nyi instr
    
    let execPUSH instr = 
        match instr.Args with
        // push reg16  15  1   push ax
        | [ ArgRegister16 r ] -> (getReg16 r >>= push) *> ns
        // push mem16  24EA    2 to 4  push word ptr [bx]
        | [ ArgDereference16 dref ] -> (readMem16 instr dref >>= push) *> ns
        // push segreg 14  1   push ds
        | [ ArgRegisterSeg r ] -> (getRegSeg r >>= push) *> ns
        | _ -> nyi instr
    
    let execPOP instr =
        match instr.Args with
        // pop reg16   12  1   pop cx
        | [ ArgRegister16 r ] -> (pop >>= setReg16 r) *> ns
        // pop mem16   25EA    2 to 4  pop word ptr [si1]
        | [ ArgDereference16 dref ] -> (pop >>= writeMem16 instr dref) *> ns
        // pop segreg (not CS) 12  1   pop es
        | [ ArgRegisterSeg r ] -> (pop >>= setRegSeg r) *> ns
        | _ -> nyi instr

    let execPUSHF _ = (getRegFlags >>= push) *> ns
    
    let execPOPF _ = (pop >>= setRegFlags) *> ns
    
    let coreSTOSX getAcc write n = 
        let writeToESDI v = (@|@) <!> getRegSeg ES <*> getReg16 DI >>= write v
        
        let updateDI = 
            getFlag Flags.DF >>= (fun df -> 
            let update = 
                if df then (-)
                else (+)
            getReg16 DI >>= (Prelude.flip update n >> setReg16 DI))
        
        let goBack = getLogicalInstrStart >>= (Some >> State.returnM)
        Prelude.tuple2 <!> getRepetitionType <*> getReg16 CX >>= (function 
        | Some _, 0us -> ns
        | None, 0us -> (getAcc >>= writeToESDI) *> updateDI *> ns
        | None, _ -> (getAcc >>= writeToESDI) *> updateDI *> ns
        | Some _, cx -> (getAcc >>= writeToESDI) *> updateDI *> (setReg16 CX (cx - 1us)) *> goBack)
    
    let coreSTOSB = coreSTOSX (getReg8 AL) writeWord8 1us
    let inline execSTOSB _ = coreSTOSB
    let coreSTOSW = coreSTOSX (getReg16 AX) writeWord16 2us
    let inline execSTOSW _ = coreSTOSW
    
    let coreSCASX getAcc read sub n = 
        let readFromESDI = (@|@) <!> getRegSeg ES <*> getReg16 DI >>= read
        let subAccESDI = Prelude.tuple2 <!> getAcc <*> readFromESDI >>= sub
        
        let updateDI = 
            getFlag Flags.DF >>= (fun df -> 
            let update = 
                if df then (-)
                else (+)
            getReg16 DI >>= (Prelude.flip update n >> setReg16 DI))
        
        let goBack = getLogicalInstrStart >>= (Some >> State.returnM)
        
        let whileDf f = 
            getFlag Flags.ZF >>= fun zf -> 
                if f zf then goBack
                else ns
        Prelude.tuple2 <!> getRepetitionType <*> getReg16 CX >>= (function 
        | Some _, 0us -> ns
        | None, 0us -> subAccESDI *> updateDI *> ns
        | None, _ -> subAccESDI *> updateDI *> ns
        | Some WhileZero, cx -> subAccESDI *> updateDI *> (setReg16 CX (cx - 1us)) *> whileDf id
        | Some WhileNotZero, cx -> subAccESDI *> updateDI *> (setReg16 CX (cx - 1us)) *> whileDf not)
    
    let coreSCASB = coreSCASX (getReg8 AL) readWord8 setSub8Flags 1us
    let inline execSCASB _ = coreSCASB
    let coreSCASW = coreSCASX (getReg16 AX) readWord16 setSub16Flags 2us
    let inline execSCASW _ = coreSCASW
