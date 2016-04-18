﻿namespace Lib.Parser

module TextInput = 
    open System
    
    type Position = 
        { Offset : int }
        override x.ToString() = sprintf "(0, %i)" x.Offset
    
    let initialPos = { Offset = 0 }
    let incrLine pos = { Offset = pos.Offset + 1 }
    
    type InputState = 
        { Bytes : byte []
          Position : Position }
        override x.ToString() = sprintf "%O %s" x.Position (new String(x.Bytes |> Array.map char))
    
    /// fromStr :: string -> InputState
    let fromStr s = 
        let lines = 
            match s with
            | s when String.IsNullOrEmpty(s) -> [||]
            | s -> 
                s
                |> Seq.map byte
                |> Seq.toArray
        { Bytes = lines
          Position = initialPos }
    
    /// nextByte :: InputState -> InputState * byte option
    let nextByte input = 
        if input.Position.Offset >= input.Bytes.Length then input, None
        else 
            let c = input.Bytes.[input.Position.Offset]
            { input with Position = incrLine input.Position }, Some c
    
    type ParserPosition = 
        { CurrentBytes : byte []
          CurrentOffset : int }
        override x.ToString() = sprintf "(%i, %i): %s" 0 x.CurrentOffset (new String(x.CurrentBytes |> Array.map char))
    
    let parserPositionFromInputState input = 
        { CurrentBytes = input.Bytes
          CurrentOffset = input.Position.Offset }