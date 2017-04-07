namespace Lib.CPU.Execution

module Control = 
    open YaFunTK
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let getAndIncrIPIf n flg = 
        if flg then 
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
        | [ ArgOffset(w16) ] -> 
            coreCALLRelative instr.Length w16
        | [ ArgRegister16 r ] -> 
            getReg16 r >>= (coreCALLIP instr.Length)
        | [ ArgDereference dref ] ->
            (addressFromDref instr dref >>= readWord16) >>= (coreCALLIP instr.Length)
        | [ ArgAddress addr ] ->
            coreCALLCSIP instr.Length addr
        | _ -> nyi instr

    let private decSP c = getReg16 SP >>= ((+) c >> State.returnM) >>= setReg16 SP

    let execRET instr = 
        let updateIP = fun csip ip -> { csip with Offset = ip } |> Some |> State.returnM
        match instr.Args with
        | [ ] -> 
            getCSIP >>= (fun csip -> pop >>= updateIP csip) 
        | [ ArgImmediate(W16 c) ] ->
            getCSIP >>= (fun csip -> pop <* (decSP c) >>= updateIP csip) 
        | _ -> nyi instr

    let execRETF instr = 
        let popIPCS = Prelude.flip (@|@) <!> pop <*> pop
        match instr.Args with
        | [ ] -> 
            popIPCS >>= (Some >> State.returnM)
        | [ ArgImmediate(W16 c) ] ->
            popIPCS <* (decSP c) >>= (Some >> State.returnM)
        | _ -> nyi instr

    let execHLT _ =
        setHalted *> ns