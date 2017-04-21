namespace Lib.Parser

module Core = 
    open TextInput
    open YaFunTK
    open YaFunTK.Result
    
    type ParserLabel = string
    
    type ParserError = 
        | MoreInputNeeded
        | ParserError of string
        override x.ToString() =
            match x with
            | MoreInputNeeded -> "Need more input"
            | ParserError msg -> msg
    
    type ParserResult<'a> = Result<'a, ParserLabel * ParserError * ParserPosition>
    
    type Parser<'T, 'U> = InputState<'U> -> ParserResult<'T * InputState<'U>>
    
    /// satisfy :: (byte -> bool) -> ParserLabel -> Parser<byte>
    let satisfy predicate label = 
        let innerFn1 is = 
            let is', byteOpt = nextByte is
            match byteOpt with
            | None -> 
                Failure(label, MoreInputNeeded, parserPositionFromInputState is)
            | Some first -> 
                if predicate first then Success(first, is')
                else 
                    let msg = sprintf "Unexpected '%c'" (first |> char) |> ParserError
                    Failure(label, msg, parserPositionFromInputState is)
        innerFn1 : Parser<_, _>
    
    /// runOnInput :: Parser<'a> -> InputState -> Result<'a * InputState>
    let runOnInput parser input = parser input
    
    /// run :: Parser<'a> -> string -> Result<'a * InputState>
    let run parser ius input = parser(fromStr ius input)
    
    let (<@>) (p : Parser<_, _>) label : Parser<_, _> = 
#if TRACE_PARSER
        let innerFn2 (is : InputState<_>) = 
            Lib.Common.dprintfn "%A: Entering %s" is.Position label
            let ret = runOnInput p is
            Lib.Common.dprintfn "%A: Leaving %s (%O)" is.Position label ret
            ret
        innerFn2 : Parser<_, _>
#else
        p
#endif
    
    /// setLabel :: string -> Parser<'a> -> Parser<'a>
    let setLabel pa label = 
        let innerFn3 str = 
            match runOnInput pa str with
            | Success a -> Success a
            | Failure(_, m, p) -> Failure(label, m, p)
        innerFn3 : Parser<_, _>
    
    /// <?> :: Parser<'a> -> string -> Parser<'a>
    let (<?>) = setLabel

    /// getPosition :: Parser<Position, 'a>
    let getPosition = 
        let innerFn4 is =
            Success(is.Position, is)
        innerFn4 : Parser<_, _>

    /// getInputChunk :: Parser<byte[], 'a>
    let getInputChunk s c = 
        let innerFn5 is =
            Success(getInputChunk is s c, is)
        innerFn5 : Parser<_, _>

    /// setUserState :: ('a -> 'a) -> Parser<'a, 'a>
    let setUserState f = 
        let innerFn6 is =
            let is' = { is with UserState = f is.UserState }
            Success(is'.UserState, is')
        innerFn6 : Parser<_, _>

    /// getUserState :: Parser<'a, 'a>
    let getUserState = 
        let innerFn7 is =
            Success(is.UserState, is)
        innerFn7 : Parser<_, _>
    
    /// printResult :: Result<'a> -> string
    let printResult = 
        function 
        | Success(a, is) -> sprintf "%A [State: %O]" a is
        | Failure(l, m, p) -> sprintf "%s: Error parsing %s. %O" (p.ToString()) l m

    module ParserResult = 
        let inline returnM x = ParserResult<_>.DoReturn(x)

        let inline bind f xResult = ParserResult<_>.DoBind(f, xResult) 

        let inline private (>>=) x f = bind f x

        let inline map f xResult = ParserResult<_>.DoMap(f, xResult)

        let inline (<!>) f v = map f v
        let inline (|>>) x f = map f x

        let inline apply fResult xResult = ParserResult<_>.DoApply(fResult, xResult)

        let inline private (<*>) f v = apply f v

        let inline lift2 f x1 x2 = returnM f <*> x1 <*> x2
