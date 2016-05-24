namespace Lib.CPU.Execution

module Logic = 
    open FSharpx
    open FSharpx.Functional
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let flagLog16 (w16 : Word16) = flagSZP16 w16 *> (setFlag CF false) *> (setFlag OF false)
    
    let opXor16 v1v2 = 
        let res = v1v2 ||> (^^^)
        flagLog16 res *> (res |> State.returnM)
    
    let execNOT instr = 
        match instr.Args with
        | [ ArgRegister16 r ] -> ((~~~) <!> getReg16 r >>= setReg16 r) *> ns
        | _ -> failwithnyi instr
    
    let execSHL instr = 
        match instr.Args with
        | [ ArgRegister16 AX; ArgConstant w8 ] -> 
            let folder acc _ = 
                acc 
                >>= (fun v -> setFlag CF (v &&& 0x8000us <> 0us) *> setReg16 AX ((v <<< 1) &&& 0xFFFFus) *> getReg16 AX)
            let doShl = [ 1..(int) w8 ] |> List.fold folder (getReg16 AX)
            ((Prelude.tuple2 <!> doShl <*> getFlag CF) 
             >>= (fun (s, cf) -> (setFlag OF ((w8 = 1uy) && (cf = ((s >>> 15) = 1us)))) *> flagSZP16 s)) *> ns
        | _ -> failwithnyi instr
    
    let execXOR instr = 
        match instr.Args with
        | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> getReg16 r2
             >>= opXor16
             >>= setReg16 r1)
            *> ns
        | [ ArgRegister16 r1; ArgImmediate(W16 w16) ] -> 
            (Prelude.tuple2 <!> getReg16 r1 <*> (w16 |> State.returnM)
             >>= opXor16
             >>= setReg16 r1)
            *> ns
        | _ -> failwithnyi instr
