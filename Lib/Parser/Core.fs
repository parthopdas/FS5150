﻿namespace Lib.Parser

module Core = 
    open TextInput
    
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
            | Success a -> sprintf "%O" a
            | Failure(l, e, p) -> sprintf "... Error: %O %O %A" l e p.CurrentOffset
    
    type Parser<'T, 'U> = InputState<'U> -> Result<'T * InputState<'U>>
    
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

    module Result = 
        open FSharpx.Functional

        /// unit :: 'a -> Result<'a>
        let returnM x = 
            Success x

        /// bind :: ('a -> Result<'b>) -> Result<'a> -> Result<'b>
        let bind f xResult = 
            match xResult with
            | Success x ->
                f x
            | Failure(l, e, p) -> Failure(l, e, p)

        let (>>=) x f = bind f x

        /// map :: ('a -> 'b) -> Result<'a> -> Result<'b>
        let map f xResult = 
            xResult >>= (f >> returnM) 

        let (<!>) = map
        let (|>>) x f = map f x

        /// apply :: Result<('a -> 'b)> -> Result<'a> -> Result<'b>
        let apply fResult xResult = 
            fResult >>= (fun f -> xResult >>= (f >> returnM)) 

        let (<*>) = apply

        let lift2 f x1 x2 = returnM f <*> x1 <*> x2

        let fold fS fF = function
            | Success a -> fS a
            | Failure (l, e, p) -> fF (l, e, p)

        let ( *>* ) r1 r2 = Prelude.tuple2 <!> r1 <*> r2

        let ( *> ) r1 r2 = Prelude.tuple2 <!> r1 <*> r2 >>= (snd >> returnM)

        let ( <* ) r1 r2 = Prelude.tuple2 <!> r1 <*> r2 >>= (fst >> returnM)

            
