namespace Lib.CPU.Execution

module Data = 
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    
    let execMOV instr = 
        match instr.Args with
        | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> (setReg8 r c) *> ns
        | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> (setReg16 r c) *> ns
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> ((getReg16 r2) >>= (setReg16 r1)) *> ns
        | [ ArgRegisterSeg r1; ArgRegister16 r2 ] -> ((getReg16 r2) >>= (setRegSeg r1)) *> ns
        | [ ArgRegister16 r1; ArgRegisterSeg r2 ] -> ((getRegSeg r2) >>= (setReg16 r1)) *> ns
        | [ ArgDereference dref; ArgImmediate(W16 c) ] -> 
            (createAddr <!> (getSegOverrideForEA instr.UseSS >>= getRegSeg) <*> getEA dref >>= writeWord16 c) *> ns
        | [ ArgRegister16 r; ArgDereference dref ] -> 
            (createAddr <!> (getSegOverrideForEA instr.UseSS >>= getRegSeg) <*> getEA dref >>= readWord16 >>= setReg16 r) *> ns
        | [ ArgDereference dref; ArgRegister16 r ] -> 
            let regReg = getReg16 r
            let setMem = fun v -> (createAddr <!> (getSegOverrideForEA instr.UseSS >>= getRegSeg) <*> getEA dref >>= writeWord16 v)
            (regReg >>= setMem) *> ns
        | _ -> nyi instr
    
    let execOUT instr = 
        match instr.Args with
        | [ ArgImmediate(W8 c); ArgRegister8 r ] -> (getReg8 r >>= portWrite !<>c) *> ns
        | [ ArgRegister16 pno; ArgRegister8 v ] -> 
            (getReg16 pno >>= (fun pno -> getReg8 v >>= portWrite pno)) *> ns
        | _ -> nyi instr
    
    let execXS instr = 
        match instr.Mneumonic with
        | "CS:" -> setSegOverride CS *> ns
        | "DS:" -> setSegOverride DS *> ns
        | "ES:" -> setSegOverride ES *> ns
        | "SS:" -> setSegOverride SS *> ns
        | _ -> nyi instr
