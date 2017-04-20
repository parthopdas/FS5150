namespace Lib.Chips.I8088.Execution

(* 
    MOV PUSH POP XCHG XLAT/XLATB IN OUT LEA LDS LE LAHF SAHF PUSHF POPF 
*)

module Data = 
    open FSharpx
    open FSharpx.State
    open Lib.Chips.I8088.Execution.Common
    open Lib.Chips.I8088.InstructionSet
    
    let execMOV i = 
        match i.Args with
        // mov reg8,reg8    2    2    mov ch,al
        | [ ArgRegister8 r1; ArgRegister8 r2 ] -> getReg8 r2 >>= setReg8 r1
        // mov [mem8],reg8    9EA    2 to 4    mov [bx10h],dh
        | [ ArgDereference8 dref; ArgRegister8 r ] -> getReg8 r >>= writeMem8 i dref
        // mov reg8,[mem8]    8EA    2 to 4    mov bl,[si]
        | [ ArgRegister8 r; ArgDereference8 dref ] -> readMem8 i dref >>= setReg8 r
        // mov reg16,reg16    2    2    mov ax,dx
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> getReg16 r2 >>= setReg16 r1
        // mov [mem16],reg16    13EA    2 to 4    mov [WordVar],cx
        | [ ArgDereference16 dref; ArgRegister16 r ] -> getReg16 r >>= writeMem16 i dref
        // mov reg16,[mem16]    12EA    2 to 4    mov bx,[Tablebx]
        | [ ArgRegister16 r; ArgDereference16 dref ] -> readMem16 i dref >>= setReg16 r
        // mov reg8,immed8    4    2    mov dl,1
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> setReg8 r c
        // mov [mem8],immed8    10EA    3 to 5    mov [ByteVar],1
        | [ ArgDereference8 dref; ArgImmediate(W8 c) ] -> writeMem8 i dref c
        // mov reg16,immed16    4    3    mov ax,88h
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> setReg16 r c
        // mov [mem16],immed16    14EA    4 to 6    mov [WordVar],1000h
        | [ ArgDereference16 dref; ArgImmediate(W16 c) ] -> writeMem16 i dref c
        // mov al,[mem8] (direct)    10    3    mov al,[Flag]
        // mov [mem8],al (direct)    10    3    mov [ByteVar],al
        // mov ax,[mem16] (direct)    14    3    mov ax,[WordVar]
        // mov [mem16],ax (direct)    14    3    mov [Count],ax
        // mov segreg,reg16    2    2    mov es,ax
        | [ ArgRegisterSeg r1; ArgRegister16 r2 ] -> getReg16 r2 >>= setRegSeg r1
        // mov segreg,[mem16]    12EA    2 to 4    mov ds,[DataPtrsbx]
        | [ ArgRegisterSeg r; ArgDereference16 dref ] -> readMem16 i dref >>= setRegSeg r
        // mov reg16,segreg    2    2    mov dx,ds
        | [ ArgRegister16 r1; ArgRegisterSeg r2 ] -> getRegSeg r2 >>= setReg16 r1
        // mov [mem16],segreg    13EA    2 to 4    mov [StackSeg],ss
        | [ ArgDereference16 dref; ArgRegisterSeg r ] -> getRegSeg r >>= writeMem16 i dref
        | _ -> nyi i
        >>. ns
    
    let execIN i = 
        match i.Args with
        // in al,dx 8   1   in al,dx
        | [ ArgRegister8 AL; ArgRegister16 DX ] -> nyi
        // in al,immed8 10  2   in al,1
        | [ ArgRegister8 AL; ArgImmediate(W8 _) ] -> nyi
        // in ax,dx 12  1   in ax,dx
        | [ ArgRegister16 AX; ArgRegister16 DX ] -> nyi
        // in ax,immed8 14  2   in ax,92h    
        | [ ArgRegister16 AX; ArgImmediate(W8 _) ] -> nyi
        | _ -> nyi i
    
    let execOUT i = 
        match i.Args with
        // out dx,al    8   1   out dx,al
        | [ ArgRegister16 DX; ArgRegister8 AL ] -> portWrite8 <!> getReg16 DX <*> getReg8 AL
        // out immed8,al    10  2   out 21h,al
        | [ ArgImmediate(W8 c); ArgRegister8 AL ] -> portWrite8 <!> (!<>c |> State.returnM) <*> getReg8 AL
        // out dx,ax    12  1   out dx,ax
        | [ ArgRegister16 DX; ArgRegister16 AX ] -> portWrite16 <!> getReg16 DX <*> getReg16 AX
        // out immed8,ax    14  2   out 10,ax
        | [ ArgImmediate(W8 c); ArgRegister16 AX ] -> portWrite16 <!> (!<>c |> State.returnM) <*> getReg16 AX
        | _ -> nyi i
        >>= id
        >>. ns
    
    let execPUSH i = 
        match i.Args with
        // push reg16  15  1   push ax
        | [ ArgRegister16 r ] -> getReg16 r >>= push
        // push mem16  24EA    2 to 4  push word ptr [bx]
        | [ ArgDereference16 dref ] -> readMem16 i dref >>= push
        // push segreg 14  1   push ds
        | [ ArgRegisterSeg r ] -> getRegSeg r >>= push
        | _ -> nyi i
        >>. ns
    
    let execPOP i = 
        match i.Args with
        // pop reg16   12  1   pop cx
        | [ ArgRegister16 r ] -> pop >>= setReg16 r
        // pop mem16   25EA    2 to 4  pop word ptr [si1]
        | [ ArgDereference16 dref ] -> pop >>= writeMem16 i dref
        // pop segreg (not CS) 12  1   pop es
        | [ ArgRegisterSeg r ] -> pop >>= setRegSeg r
        | _ -> nyi i
        >>. ns
    
    let execPUSHF _ = getRegFlags >>= push
                      >>. ns

    let execPOPF _ = pop >>= setRegFlags
                     >>. ns
