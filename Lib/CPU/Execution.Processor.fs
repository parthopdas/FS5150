namespace Lib.CPU.Execution

module Processor = 
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let execCLI instr = 
        match instr.Args with
        | [] -> setFlag IF false *> ns
        | _ -> nyi instr
    
    let execCLD instr = 
        match instr.Args with
        | [] -> setFlag DF false *> ns
        | _ -> nyi instr
