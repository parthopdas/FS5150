namespace Lib.Parser

module TextInput = 
    open System
        
    type InputState<'a> = 
        { Bytes : byte []
          Position : int
          UserState : 'a }
        override x.ToString() = sprintf "%O %s" x.Position (new String(x.Bytes |> Array.map char))

    let getInputChunk is s e =
        Array.sub is.Bytes s (e - s)
    
    /// fromStr :: string -> InputState
    let fromStr us s = 
        let lines = 
            match s with
            | s when String.IsNullOrEmpty(s) -> [||]
            | s -> 
                s
                |> Seq.map byte
                |> Seq.toArray
        { Bytes = lines
          Position = 0
          UserState = us }
    
    /// fromBytes :: byte[] -> InputState
    let fromBytes us bytes = 
        { Bytes = bytes
          Position = 0
          UserState = us }
    
    /// nextByte :: InputState -> InputState * byte option
    let nextByte input = 
        if input.Position >= input.Bytes.Length then input, None
        else 
            let c = input.Bytes.[input.Position]
            { input with Position = input.Position + 1 }, Some c
    
    type ParserPosition = 
        { CurrentBytes : byte []
          CurrentOffset : int }
        override x.ToString() = sprintf "%i: %s" x.CurrentOffset (new String(x.CurrentBytes |> Array.map char))
    
    let parserPositionFromInputState input = 
        { CurrentBytes = input.Bytes
          CurrentOffset = input.Position }
