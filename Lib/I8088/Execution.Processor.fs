﻿namespace Lib.Chips.I8088.Execution
(* 
    STC CLC CMC STD CLD STI CLI HLT WAIT ESC LOCK NOP
*)
module Processor = 
    open FSharpx
    open FSharpx.State
    open Lib.Chips.I8088.Execution.Common
    open Lib.Chips.I8088
    
    let inline execCMC _ = ((getFlag Flags.CF) >>= (not >> setFlag Flags.CF)) *> ns

    let inline execCLC _ = setFlag Flags.CF false *> ns
    let inline execSTC _ = setFlag Flags.CF true *> ns

    let inline execCLD _ = setFlag Flags.DF false *> ns
    let inline execSTD _ = setFlag Flags.DF true *> ns

    let inline execCLI _ = setFlag Flags.IF false *> ns
    let inline execSTI _ = setFlag Flags.IF true *> ns

    let inline execHLT _ =
        setHalted *> ns

    let inline execNOP _ = ns
