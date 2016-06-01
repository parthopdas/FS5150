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
            | Success a -> sprintf "%O" a
            | Failure(l, e, p) -> sprintf "... Error: %O %O %A" l e p.CurrentOffset
    
    type Parser<'T, 'U> = 
        { ParserFn : InputState<'U> -> Result<'T * InputState<'U>>
          Label : ParserLabel }
    
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
        { ParserFn = innerFn1
          Label = label }
    
    /// runOnInput :: Parser<'a> -> InputState -> Result<'a * InputState>
    let runOnInput parser input = parser.ParserFn input
    
    /// run :: Parser<'a> -> string -> Result<'a * InputState>
    let run parser ius input = parser.ParserFn(fromStr ius input)
    
    let (<@>) (p : Parser<_, _>) label : Parser<_, _> = 
        (*let innerFn2 (is : InputState<_>) = 
            dprintfn "%A: Entering %s" is.Position label
            let ret = runOnInput p is
            dprintfn "%A: Leaving %s (%O)" is.Position label ret
            ret
        { p with ParserFn = innerFn2 }*)
        p
    
    /// setLabel :: string -> Parser<'a> -> Parser<'a>
    let setLabel pa label = 
        let innerFn3 str = 
            match runOnInput pa str with
            | Success a -> Success a
            | Failure(_, m, p) -> Failure(label, m, p)
        { ParserFn = innerFn3
          Label = label }
    
    /// <?> :: Parser<'a> -> string -> Parser<'a>
    let (<?>) = setLabel

    /// getPosition :: Parser<Position, 'a>
    let getPosition = 
        let innerFn4 is =
            Success(is.Position, is)
        { ParserFn = innerFn4
          Label = "getPosition" }

    /// getInputChunk :: Parser<byte[], 'a>
    let getInputChunk s c = 
        let innerFn5 is =
            Success(getInputChunk is s c, is)
        { ParserFn = innerFn5
          Label = "getPosition" }

    /// setUserState :: ('a -> 'a) -> Parser<'a, 'a>
    let setUserState f = 
        let innerFn6 is =
            let is' = { is with UserState = f is.UserState }
            Success(is'.UserState, is')
        { ParserFn = innerFn6
          Label = "updateUserState" }

    /// getUserState :: Parser<'a, 'a>
    let getUserState = 
        let innerFn7 is =
            Success(is.UserState, is)
        { ParserFn = innerFn7
          Label = "getUserState" }
    
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

            
