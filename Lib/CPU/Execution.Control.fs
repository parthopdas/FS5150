﻿namespace Lib.CPU.Execution

module Control = 
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let getAndIncrIPIf n flg = 
        if flg then 
            getCSIP >>= (incrAddress n
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
            getCSIP >>= (incrAddress (instr.Length + w16)
                         >> Some
                         >> State.returnM)
        | _ -> failwithnyi instr
    
    let execJB instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag CF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> failwithnyi instr
    
    let execJNB instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag CF >>= (not >> getAndIncrIPIf (instr.Length + w16))
        | _ -> failwithnyi instr
    
    let execJO instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag OF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> failwithnyi instr
    
    let execJNO instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag OF >>= (not >> getAndIncrIPIf (instr.Length + w16))
        | _ -> failwithnyi instr
    
    let execJS instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag SF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> failwithnyi instr
    
    let execJZ instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag ZF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> failwithnyi instr
    
    let execJNZ instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag ZF >>= (not >> getAndIncrIPIf (instr.Length + w16))
        | _ -> failwithnyi instr
    
    let execJPE instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag PF >>= getAndIncrIPIf (instr.Length + w16)
        | _ -> failwithnyi instr
    
    let execJPO instr = 
        match instr.Args with
        | [ ArgOffset(w16) ] -> getFlag PF >>= (not >> getAndIncrIPIf (instr.Length + w16))
        | _ -> failwithnyi instr
