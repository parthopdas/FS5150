namespace Lib.CPU.Execution
(* 
    CALL RET 
    JMPJA/JNBE JAE/JNB JB/JNAE JBE/JNA JC JE/JZ JG/JNLE JGE/JNL JL/JNGE JLE/JNG JNC JNE/JNZ JNO JNP/JPO JNS JOJP/JPE JS
    LOOP LOOPE/LOOPZ LOOPNE/LOOPNZ JCXZ 
    INT INTO IRET
*)
module Control = 
    open YaFunTK
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let getAndIncrIPIf n c = 
        if c then 
            getCSIP >>= (Prelude.flip (|++) n
                         >> Some
                         >> State.returnM)
        else None |> State.returnM
    
    let execJMP instr = 
        match instr.Args with
        | [ ArgAddress a ] -> 
            a
            |> Some
            |> State.returnM
        | [ ArgOffset(w16) ] -> 
            getCSIP >>= (Prelude.flip (|++) (instr.Length + w16)
                         >> Some
                         >> State.returnM)
        | _ -> nyi instr
    
    let execJB instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.CF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> nyi instr
    
    let execJNB instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.CF >>= (not >> getAndIncrIPIf (instr.Length + w16))
        | _ -> nyi instr
    
    let execJO instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.OF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> nyi instr
    
    let execJNO instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.OF >>= (not >> getAndIncrIPIf (instr.Length + w16))
        | _ -> nyi instr
    
    let execJS instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.SF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> nyi instr
    
    let execJZ instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.ZF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> nyi instr
    
    let execJNZ instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.ZF >>= (not >> getAndIncrIPIf (instr.Length + w16))
        | _ -> nyi instr
    
    let execJPE instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.PF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> nyi instr
    
    let execJPO instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag Flags.PF >>= (not >> getAndIncrIPIf (instr.Length + w16))
        | _ -> nyi instr
    
    let execJCXZ instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getReg16 CX >>= ((=) 0us >> getAndIncrIPIf (instr.Length + w16))
        | _ -> nyi instr
    
    let execXS sreg _ = 
        setSegOverride sreg *> ns
    
    let execREPX rt _ = 
        setRepetitionType rt *> ns
        
    let inline private coreCALLRelative ilen w16 =
        getCSIP >>= (fun csip -> 
                     let retAddr = csip |++ ilen
                     push retAddr.Offset *> ((retAddr |++ w16) |> Some |> State.returnM))

    let inline private coreCALLIP ilen w16 =
        getCSIP >>= (fun csip -> 
                     let retAddr = csip |++ ilen
                     push retAddr.Offset *> ({csip with Offset = w16} |> Some |> State.returnM))
    
    let inline private coreCALLCSIP ilen newCSIP =
        (getCSIP >>= (fun csip -> 
                      let retAddr = csip |++ ilen
                      push retAddr.Segment *> push retAddr.Offset)) *> (newCSIP |> Some |> State.returnM)

    let execCALL instr = 
        match instr.Args with
        // call disp16  23  3   call near ptr NearTarget
        | [ ArgOffset(w16) ] -> 
            coreCALLRelative instr.Length w16
        // call reg16   20  2   call bx
        | [ ArgRegister16 r ] -> 
            getReg16 r >>= (coreCALLIP instr.Length)
        // call [mem16] 29EA    2 to 4  call word ptr [Vecssi]
        | [ ArgDereference16 dref ] ->
            (readMem16 instr dref) >>= (coreCALLIP instr.Length)
        // call segment:offset  36  5   call far ptr FarTarget
        | [ ArgAddress addr ] ->
            coreCALLCSIP instr.Length addr
        // call [mem32] 53EA    2 to 4  call dword ptr [FarVec]
        // ??? - Support parsing this instruction and then add the tests for it
        | _ -> nyi instr

    let private decSP c = getReg16 SP >>= ((+) c >> State.returnM) >>= setReg16 SP

    let execRET instr = 
        let updateIP = fun csip ip -> { csip with Offset = ip } |> Some |> State.returnM
        match instr.Args with
        // retn 20  1   ret (in near proc)
        | [ ] -> 
            getCSIP >>= (fun csip -> pop >>= updateIP csip) 
        // retn immed16 24  3   retn 10
        | [ ArgImmediate(W16 c) ] ->
            getCSIP >>= (fun csip -> pop <* (decSP c) >>= updateIP csip) 
        | _ -> nyi instr

    let execRETF instr = 
        let popIPCS = Prelude.flip (@|@) <!> pop <*> pop
        match instr.Args with
        // retf 34  1   retf
        | [ ] -> 
            popIPCS >>= (Some >> State.returnM)
        // retf immed16 33  3   ret 512 (in far proc)
        | [ ArgImmediate(W16 c) ] ->
            popIPCS <* (decSP c) >>= (Some >> State.returnM)
        | _ -> nyi instr

    module LOOPX =
        let doLoop instrLen off ccond =
            let cond = Prelude.tuple2 <!> getFlag Flags.ZF <*> getReg16 CX >>= (fun (zf, cx) -> (cx <> 0us && ccond zf) |> State.returnM)
            (getReg16 CX >>= ((+) 0xFFFFus >> setReg16 CX)) 
            *> (cond >>= getAndIncrIPIf (instrLen + off))

    let execLOOP instr = 
        match instr.Args with
        // loop disp8    17 (CX<>0)/5 (CX=0)    2    loop WaitLoop
        | [ ArgOffset(off) ] -> LOOPX.doLoop instr.Length off (Prelude.ct true)
        | _ -> nyi instr

    let execLOOPNZ instr = 
        match instr.Args with
        // loopnz disp8    19 (CX<>0 and ZF=0)/5 (CX=0 or ZF=1)    2    loopnz PollLp
        | [ ArgOffset(off) ] -> LOOPX.doLoop instr.Length off not
        | _ -> nyi instr

    let execLOOPZ instr = 
        match instr.Args with
        // loopz disp8    18 (CX<>0 and ZF=1)/6 (CX=0 or ZF=0)    2    loopz MaxWtLp
        | [ ArgOffset(off) ] -> LOOPX.doLoop instr.Length off id
        | _ -> nyi instr

    let execINT _ = ns