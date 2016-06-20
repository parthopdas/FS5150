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

    let coreSTOSX getAcc write n = 
        let writeToESDI v = (@|@) <!> getRegSeg ES <*> getReg16 DI >>= write v
            
        let updateDI = 
            getFlag DF >>= (fun df -> 
            let update = 
                if df then (-)
                else (+)
            getReg16 DI >>= (Prelude.flip update n >> setReg16 DI))
            
        let goBack = getLogicalInstrStart >>= (Some >> State.returnM)
        
        Prelude.tuple2 <!> getRepetitionType <*> getReg16 CX 
        >>= (function 
            | Some _, 0us -> ns
            | None, 0us -> (getAcc >>= writeToESDI) *> updateDI *> ns
            | None, _ -> (getAcc >>= writeToESDI) *> updateDI *> ns
            | Some _, cx -> (getAcc >>= writeToESDI) *> updateDI *> (setReg16 CX (cx - 1us)) *> goBack)

    let execSTOSB _ = coreSTOSX (getReg8 AL) writeWord8 1us 
    
    let execSTOSW _ = coreSTOSX (getReg16 AX) writeWord16 2us


