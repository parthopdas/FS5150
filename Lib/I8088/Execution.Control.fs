namespace Lib.Chips.I8088.Execution
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
    open Lib.Chips.I8088.Execution.Common
    open Lib.Chips.I8088.InstructionSet
    open Lib.Chips.I8088
    
    let getAndIncrIPIf n c = 
        if c then 
            getCSIP >>= (Prelude.flip (|++) n
                         >> Some
                         >> State.returnM)
        else None |> State.returnM

    let execJXXX cond instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> cond >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> nyi instr

    // ja disp8     Jump above                  CF=0 and ZF=0   ja OutOfRange
    // jnbe disp8   Jump not below or equal     CF=0 and ZF=0   jnbe TooHigh
    let execJA = 
        state {
            let! cf = getFlag Flags.CF
            let! zf = getFlag Flags.ZF
            return (not cf && not zf) 
        } |> execJXXX

    // jb disp8     Jump below                  CF=1    jb TooLow
    // jc disp8     Jump Carry flag set         CF=1    jc NextTest
    // jnae disp8   Jump not above or equal     CF=1    jnae Skip1
    let execJB = 
        state {
            return! getFlag Flags.CF
        } |> execJXXX
    
    // jbe disp8    Jump below or equal         CF=1 or ZF=1    jbe Exit
    // jna disp8    Jump not above              CF=1 or ZF=1    jna NotAbove
    let execJBE =
        state {
            let! cf = getFlag Flags.CF
            let! zf = getFlag Flags.ZF
            return (cf || zf) 
        } |> execJXXX


    // jnle disp8   Jump not less than or equal     ZF=0 and SF=OF  jnle ShortLab
    // jg disp8     Jump greater                    ZF=0 and SF=OF  jg Greater
    let execJG =
        state {
            let! zf = getFlag Flags.ZF
            let! sf = getFlag Flags.SF
            return (not zf && not sf) 
        } |> execJXXX

    // jge disp8    Jump greater than or equal      SF=OF   jge GtThanEq
    // jnl disp8    Jump not less than              SF=OF   jnl NotLess
    let execJGE = 
        state {
            let! sf = getFlag Flags.SF
            let! ofl = getFlag Flags.OF
            return (sf = ofl) 
        } |> execJXXX

    // jl disp8     Jump less than                  SF<>OF  jl IsLessThan
    // jnge disp8   Jump not greater than or equal  SF<>OF  jnge Point2
    let execJL =
        state {
            let! sf = getFlag Flags.SF
            let! ofl = getFlag Flags.OF
            return (sf <> ofl) 
        } |> execJXXX

    // jle disp8    Jump less than or equal         ZF=1 or SF<>OF  jle LessThanEq
    // jng disp8    Jump not greater                ZF=1 or SF<>OF  jng LoopBottom
    let execJLE =
        state {
            let! zf = getFlag Flags.ZF
            let! sf = getFlag Flags.SF
            let! ofl = getFlag Flags.OF
            return (zf && (sf <> ofl)) 
        } |> execJXXX

    // jae disp8    Jump above or equal             CF=0    jae XLabel
    // jnb disp8    Jump not below                  CF=0    jnb OffTop
    // jnc disp8    Jump Carry flag not set         CF=0    jnc TryAgain
    let execJNB = 
        state {
            let! cf = getFlag Flags.CF
            return not cf 
        } |> execJXXX
    

    // jno disp8    Jump Overflow flag not set      OF=0    jno NoOverflow
    let execJNO = 
        state {
            let! ofl = getFlag Flags.OF
            return not ofl 
        } |> execJXXX
    
    // jns disp8    Jump Sign flag not set          SF=0    jns NoSign
    let execJNS = 
        state {
            let! sf = getFlag Flags.SF
            return not sf 
        } |> execJXXX

    // jne disp8    Jump not equal                  ZF=0    jne Mismatch
    // jnz disp8    Jump not zero                   ZF=0    jnz Different
    let execJNZ = 
        state {
            let! zf = getFlag Flags.ZF
            return not zf 
        } |> execJXXX
    
    // jo disp8     Jump Overflow flag set          OF=1    jo Overflow
    let execJO = 
        state {
            let! ofl = getFlag Flags.OF
            return ofl 
        } |> execJXXX
    
    // jp disp8     Jump Parity flag set            PF=1    jp ParCheck1
    // jpe disp8    Jump Parity Even                PF=1    jpe ParityEven
    let execJPE = 
        state {
            let! pf = getFlag Flags.PF
            return pf 
        } |> execJXXX
    
    // jnp disp8    Jump Parity flag not set        PF=0    jnp EndText
    // jpo disp8    Jump Parity Odd                 PF=0    jpo OddParity
    let execJPO = 
        state {
            let! pf = getFlag Flags.PF
            return not pf 
        } |> execJXXX
    
    // js disp8     Jump Sign flag set              SF=1    js Negative
    let execJS = 
        state {
            let! sf = getFlag Flags.SF
            return sf 
        } |> execJXXX
    
    // jz disp8 Jump zero                           ZF=1    jz Match
    // je disp8 Jump equal                          ZF=1    je Same
    let execJZ = 
        state {
            let! zf = getFlag Flags.ZF
            return zf 
        } |> execJXXX
        
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
    
    let execJCXZ instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getReg16 CX >>= ((=) 0us >> getAndIncrIPIf (instr.Length + w16))
        | _ -> nyi instr
    
    let execXS sreg _ = 
        setSegOverride sreg *> ns

    let execCS instr = execXS CS instr 

    let execDS instr = execXS DS instr 
    
    let execES instr = execXS ES instr 
    
    let execSS instr = execXS SS instr  
    
    let execREPX rt _ = 
        setRepetitionType rt *> ns
        
    let execREPNZ instr = execREPX WhileNotZero instr
        
    let execREPZ instr = execREPX WhileZero instr

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
            let cond = 
                (fun zf cx -> (cx <> 0us && ccond zf)) <!> getFlag Flags.ZF <*> getReg16 CX 
            getReg16 CX >>= ((+) 0xFFFFus >> setReg16 CX)
            >>. cond >>= getAndIncrIPIf (instrLen + off)

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

    let inline execINT _ = ns

    let inline execIRET _ = nyi
    
    let inline execINTO _ = nyi
