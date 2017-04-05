namespace Lib.CPU.Execution

module Processor = 
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let execCLI instr = 
        match instr.Args with
        | [] -> setFlag Flags.IF false *> ns
        | _ -> nyi instr
    
    let inline execCLD instr = 
#if PERF
        ns
#else
        match instr.Args with
        | [] -> setFlag Flags.DF false *> ns
        | _ -> nyi instr
#endif
