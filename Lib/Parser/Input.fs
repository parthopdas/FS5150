namespace Lib.Parser

module TextInput = 
    open System
    
    type Position = 
        { Line : int
          Column : int }
        override x.ToString() = sprintf "(%i, %i)" x.Line x.Column
    
    let initialPos = 
        { Line = 0
          Column = 0 }
    
    let incrCol pos = { pos with Column = pos.Column + 1 }
    
    let incrLine pos = 
        { Line = pos.Line + 1
          Column = 0 }
    
    type InputState = 
        { Lines : string []
          Position : Position }
        override x.ToString() = sprintf "%O %s" x.Position (String.Join("\n", x.Lines))
    
    /// fromStr :: string -> InputState
    let fromStr s = 
        let lines = 
            match s with
            | s when String.IsNullOrEmpty(s) -> [||]
            | s -> s.Split([| "\r"; "\n"; "\r\n" |], StringSplitOptions.None)
        { Lines = lines
          Position = initialPos }
    
    /// currentLine :: InputState -> string
    let currentLine inputState = 
        let linePos = inputState.Position.Line
        if inputState.Lines.Length > linePos then inputState.Lines.[linePos]
        else "end of file"
    
    /// nextChar :: InputState -> InputState * char option
    let nextChar input = 
        let colPos = input.Position.Column
        let linePos = input.Position.Line
        // three cases
        // 1) if line >= maxLine -> 
        //       return EOF
        if linePos >= input.Lines.Length then input, None
        // 2) if col less than line length -> 
        //       return char at colPos, increment colPos
        else 
            let currentLine = currentLine input
            if colPos < currentLine.Length then 
                let c = currentLine.[colPos]
                { input with Position = incrCol input.Position }, Some c
            // 3) if col at line length -> 
            //       return NewLine, increment linePos
            else { input with Position = incrLine input.Position }, Some '\n'
    
    type ParserPosition = 
        { CurrentLine : string
          Line : int
          Column : int }
        override x.ToString() = sprintf "(%i, %i): %s" x.Line x.Column x.CurrentLine
    
    let parserPositionFromInputState input = 
        { CurrentLine = currentLine input
          Line = input.Position.Line
          Column = input.Position.Column }
