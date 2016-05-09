namespace Lib.Parser

module Core = 
    open TextInput
    
    let inline dprintfn fmt = Printf.ksprintf System.Diagnostics.Debug.WriteLine fmt
    
    type ParserLabel = string
    
    type ParserError = 
        | MoreInputNeeded
        | ParserError of string
        override x.ToString() =
            match x with
            | MoreInputNeeded -> "No more input"
            | ParserError msg -> msg
    
    type Result<'a> = 
        | Success of 'a
        | Failure of ParserLabel * ParserError * ParserPosition
        override x.ToString() = 
            match x with
            | Success _ -> "... OK"
            | Failure(l, e, p) -> sprintf "... Error: %O %O %A" l e p.CurrentOffset
    
    type Parser<'T, 'U> = 
        { ParserFn : InputState<'U> -> Result<'T * InputState<'U>>
          Label : ParserLabel }
    
    /// satisfy :: (byte -> bool) -> ParserLabel -> Parser<byte>
    let satisfy predicate label = 
        let innerFn is = 
            let is', byteOpt = nextByte is
            match byteOpt with
            | None -> 
                Failure(label, MoreInputNeeded, parserPositionFromInputState is)
            | Some first -> 
                if predicate first then Success(first, is')
                else 
                    let msg = sprintf "Unexpected '%c'" (first |> char) |> ParserError
                    Failure(label, msg, parserPositionFromInputState is)
        { ParserFn = innerFn
          Label = label }
    
    /// runOnInput :: Parser<'a> -> InputState -> Result<'a * InputState>
    let runOnInput parser input = parser.ParserFn input
    
    /// run :: Parser<'a> -> string -> Result<'a * InputState>
    let run parser ius input = parser.ParserFn(fromStr ius input)
    
    let (<@>) (p : Parser<_, _>) label : Parser<_, _> = 
        let innerFn (is : InputState<_>) = 
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

    /// getPosition :: Parser<Position, 'a>
    let getPosition = 
        let innerFn is =
            Success(is.Position, is)
        { ParserFn = innerFn
          Label = "getPosition" }

    /// getInputChunk :: Parser<byte[], 'a>
    let getInputChunk s c = 
        let innerFn is =
            Success(getInputChunk is s c, is)
        { ParserFn = innerFn
          Label = "getPosition" }

    /// setUserState :: ('a -> 'a) -> Parser<'a, 'a>
    let setUserState f = 
        let innerFn is =
            let is' = { is with UserState = f is.UserState }
            Success(is'.UserState, is')
        { ParserFn = innerFn
          Label = "updateUserState" }

    /// getUserState :: Parser<'a, 'a>
    let getUserState = 
        let innerFn is =
            Success(is.UserState, is)
        { ParserFn = innerFn
          Label = "getUserState" }
    
    /// printResult :: Result<'a> -> string
    let printResult = 
        function 
        | Success(a, is) -> sprintf "%A [State: %O]" a is
        | Failure(l, m, p) -> sprintf "%s: Error parsing %s. %O" (p.ToString()) l m

    module Result = 

        /// returnR: 'a -> Result<'a>
        let returnR x = 
            Success x

        /// bindR: ('a -> Result<'b>) -> Result<'a> -> Result<'b>
        let bindR f xResult = 
            match xResult with
            | Success x ->
                f x
            | Failure(l, e, p) -> Failure(l, e, p)

        let (>>=) x f = bindR f x

        /// mapR: ('a -> 'b) -> Result<'a> -> Result<'b>
        let mapR f xResult = 
            xResult >>= (f >> returnR) 

        let (<!>) = mapR
        let (|>>) x f = mapR f x

        /// applyR: Result<('a -> 'b)> -> Result<'a> -> Result<'b>
        let applyR fResult xResult = 
            fResult >>= (fun f -> xResult >>= (f >> returnR)) 

        let (<*>) = applyR
