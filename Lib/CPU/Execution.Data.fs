namespace Lib.CPU.Execution

module Data = 
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open FSharpx.Functional
    
    let execMOV instr = 
        match instr.Args with
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> (setReg8 r c) *> ns
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> (setReg16 r c) *> ns
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> ((getReg16 r2) >>= (setReg16 r1)) *> ns
        | [ ArgRegisterSeg r1; ArgRegister16 r2 ] -> ((getReg16 r2) >>= (setRegSeg r1)) *> ns
        | [ ArgRegister16 r1; ArgRegisterSeg r2 ] -> ((getRegSeg r2) >>= (setReg16 r1)) *> ns
        | [ ArgDereference dref; ArgImmediate(W16 c) ] -> 
            (addressFromDref instr dref >>= writeWord16 c) *> ns
        | [ ArgRegister16 r; ArgDereference dref ] -> 
            (addressFromDref instr dref >>= readWord16 >>= setReg16 r) *> ns
        | [ ArgDereference dref; ArgRegister16 r ] -> 
            let regReg = getReg16 r
            let setMem = fun v -> (addressFromDref instr dref >>= writeWord16 v)
            (regReg >>= setMem) *> ns
        | _ -> nyi instr
    
    let execOUT instr = 
        match instr.Args with
        | [ ArgImmediate(W8 c); ArgRegister8 r ] -> (getReg8 r >>= portWrite !<>c) *> ns
        | [ ArgRegister16 pno; ArgRegister8 v ] -> 
            (getReg16 pno >>= (fun pno -> getReg8 v >>= portWrite pno)) *> ns
        | _ -> nyi instr

    let execPUSH instr = 
        match instr.Args with
        | [ ArgRegister16 r ] -> (getReg16 r >>= push) *> ns
        | _ -> nyi instr

    let execSTOSW _ = 
        let ifCXNot0 = 
            let writeToESDI v = (@|@) <!> getRegSeg ES <*> getReg16 DI >>= writeWord16 v
            
            let updateDI = 
                getFlag DF >>= (fun df -> 
                let update = 
                    if df then (-)
                    else (+)
                getReg16 DI >>= (Prelude.flip update 2us >> setReg16 DI))
            
            let updateCX = getReg16 CX >>= (Prelude.flip (-) 1us >> setReg16 CX)
            let getLogicalInstr = getLogicalInstrStart >>= (Some >> State.returnM)
            (getReg16 AX >>= writeToESDI) *> updateDI *> updateCX *> getLogicalInstr
        
        let ifReping _ _ = 
            getReg16 CX >>= (fun cx -> if cx <> 0us then ifCXNot0 else ns)
        
        getRepetitionType >>= Option.fold ifReping ns