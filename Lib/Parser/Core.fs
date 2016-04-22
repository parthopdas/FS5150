namespace Lib.Parser

module Core = 
    open TextInput
    
    let inline dprintfn fmt = Printf.ksprintf System.Diagnostics.Debug.WriteLine fmt
    
    type Input = InputState
    
    type ParserLabel = string
    
    type ParserError = string
    
    type Result<'a> = 
        | Success of 'a
        | Failure of ParserLabel * ParserError * ParserPosition
        override x.ToString() = 
            match x with
            | Success _ -> "... OK"
            | Failure(l, e, p) -> sprintf "... Error: %O %O %A" l e p.CurrentOffset
    
    type Parser<'T> = 
        { ParserFn : Input -> Result<'T * Input>
          Label : ParserLabel }
    
    /// satisfy :: (byte -> bool) -> ParserLabel -> Parser<byte>
    let satisfy predicate label = 
        let innerFn is = 
            let is', byteOpt = nextByte is
            match byteOpt with
            | None -> 
                let msg = "No more input"
                Failure(label, msg, parserPositionFromInputState is)
            | Some first -> 
                if predicate first then Success(first, is')
                else 
                    let msg = sprintf "Unexpected '%c'" (first |> char)
                    Failure(label, msg, parserPositionFromInputState is)
        { ParserFn = innerFn
          Label = label }
    
    /// runOnInput :: Parser<'a> -> Input -> Result<'a * Input>
    let runOnInput parser input = parser.ParserFn input
    
    /// run :: Parser<'a> -> string -> Result<'a * Input>
    let run parser input = parser.ParserFn(fromStr input)
    
    let (<@>) (p : Parser<_>) label : Parser<_> = 
        let innerFn (is : Input) = 
            dprintfn "%A: Entering %s" is.Position label
            let ret = runOnInput p is
            dprintfn "%A: Leaving %s (%O)" is.Position label ret
            ret
        { p with ParserFn = innerFn }
    
    /// setLabel :: string -> Parser<'a> -> Parser<'a>
    let setLabel pa label = 
        let innerFn str = 
            match runOnInput pa str with
            | Success a -> Success a
            | Failure(_, m, p) -> Failure(label, m, p)
        { ParserFn = innerFn
          Label = label }
    
    /// <?> :: Parser<'a> -> string -> Parser<'a>
    let (<?>) = setLabel
    
    /// printResult :: Result<'a> -> string
    let printResult = 
        function 
        | Success(a, is) -> sprintf "%A [State: %O]" a is
        | Failure(l, m, p) -> sprintf "%O: Error parsing %s. %s" p l m
