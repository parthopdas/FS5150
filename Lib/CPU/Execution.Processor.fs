namespace Lib.CPU.Execution

module Processor = 
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.PC
    
    let inline execCMC _ = ((getFlag Flags.CF) >>= (not >> setFlag Flags.CF)) *> ns

    let inline execCLC _ = setFlag Flags.CF false *> ns
    let inline execSTC _ = setFlag Flags.CF true *> ns

    let inline execCLD _ = setFlag Flags.DF false *> ns
    let inline execSTD _ = setFlag Flags.DF true *> ns

    let inline execCLI _ = setFlag Flags.IF false *> ns
    let inline execSTI _ = setFlag Flags.IF true *> ns
