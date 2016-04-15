namespace Lib.Parser

module TextInput = 
    open System
    
    type Position = 
        { Line : int }
        override x.ToString() = sprintf "(0, %i)" x.Line
    
    let initialPos = 
        { Line = 0 }
    
    let incrLine pos = 
        { Line = pos.Line + 1 }
    
    type InputState = 
        { Lines : byte[]
          Position : Position }
        override x.ToString() = sprintf "%O %s" x.Position (new String(x.Lines |> Array.map char))
    
    /// fromStr :: string -> InputState
    let fromStr s = 
        let lines = 
            match s with
            | s when String.IsNullOrEmpty(s) -> [||]
            | s -> s |> Seq.map byte |> Seq.toArray
        { Lines = lines
          Position = initialPos }
    
    /// nextChar :: InputState -> InputState * char option
    let nextChar input = 
        // three cases
        // 1) if line >= maxLine -> 
        //       return EOF
        if input.Position.Line >= input.Lines.Length then input, None
        // 2) if col less than line length -> 
        //       return char at colPos, increment colPos
        else 
            let c = input.Lines.[input.Position.Line]
            { input with Position = incrLine input.Position }, Some c
    
    type ParserPosition = 
        { CurrentLine : byte[]
          Line : int }
        override x.ToString() = sprintf "(%i, %i): %s" 0 x.Line (new String(x.CurrentLine |> Array.map char))
    
    let parserPositionFromInputState input = 
        { CurrentLine = input.Lines
          Line = input.Position.Line }
