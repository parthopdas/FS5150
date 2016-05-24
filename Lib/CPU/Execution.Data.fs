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
        | _ -> failwithnyi instr
    
    let execOUT instr = 
        match instr.Args with
        | [ ArgImmediate(W8 c); ArgRegister8 r ] -> (getReg8 r >>= outPort8to8 c) *> ns
        | [ ArgRegister16 pno; ArgRegister8 v ] -> 
            (getReg16 pno >>= (fun pno -> getReg8 v >>= (outPort8to16 pno))) *> ns
        | _ -> failwithnyi instr
