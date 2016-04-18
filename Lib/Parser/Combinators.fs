namespace Lib.Parser

module Combinators = 
    open Lib.Parser.Core
    
    /// bindP :: ('a -> Parser<'b>) -> Parser<'a> -> Parser<'b>
    let bindP f p = 
        let label = "unknown"
        
        let innerFn is = 
            match runOnInput p is with
            | Success(c, remaining) -> runOnInput (f c) remaining
            | Failure(l, msg, pos) -> Failure(l, msg, pos)
        { ParserFn = innerFn
          Label = label }
    
    /// (>>=) :: ('a -> Parser<'b>) -> Parser<'a> -> Parser<'b>
    let (>>=) p f = bindP f p
    
    /// returnP :: 'a -> Parser<'a>
    let returnP a = 
        let innerFn is = Success(a, is)
        { ParserFn = innerFn
          Label = sprintf "%A" a }
    
    /// andThen :: Parser<'a> -> Parser<'b> -> Parser<'a * 'b>
    let andThen p1 p2 = p1 >>= (fun a -> p2 >>= (fun b -> returnP (a, b)))
    
    let (.>>.) = andThen
    
    /// orElse :: Parser<'a> -> Parser<'a> -> Parser<'a>
    let orElse p1 p2 = 
        let innerFn input = 
            let result1 = runOnInput p1 input
            match result1 with
            | Failure _ -> 
                let result2 = runOnInput p2 input
                result2
            | Success(_) as s1 -> s1
        { ParserFn = innerFn
          Label = sprintf "%s orElse %s" p1.Label p2.Label }
    
    let (<|>) = orElse
    
    /// mapP :: ('a -> 'b) -> Parser<'a> -> Parser<'b>
    let mapP f = 
        f
        >> returnP
        |> bindP
    
    let (<!>) = mapP
    let (|>>) x f = mapP f x
    
    /// applyP :: Parser<'a -> b'> -> Parser<'a> -> Parser<'b>
    let applyP pf pa = pf >>= (fun f -> pa >>= (f >> returnP))
    
    let (<*>) = applyP
    
    /// sequence :: Parser<'a> list -> Parser<'a list>
    let sequence (pas : Parser<'a> list) : Parser<'a list> = 
        List.foldBack (fun e acc -> e .>>. acc |>> List.Cons) pas (returnP [])
    
    let rec private get0OrMore pa is = 
        match runOnInput pa is with
        | Success(c, remaining1) -> 
            let (cs, remaining2) = get0OrMore pa remaining1
            (c :: cs, remaining2)
        | Failure _ -> ([], is)
    
    /// many :: Parser<'a> -> Parser<'a list>
    let many (pa : Parser<'a>) : Parser<'a list> = 
        let innerFn is = Success(get0OrMore pa is)
        { ParserFn = innerFn
          Label = sprintf "many %s" pa.Label }
    
    /// many1 :: Parser<'a> -> Parser<'a list>
    let many1 (pa : Parser<'a>) : Parser<'a list> = 
        pa >>= (fun h -> many pa >>= (fun t -> { returnP (h :: t) with Label = sprintf "many1 %s" pa.Label }))
    
    /// opt :: Parser<'a> -> Parser<'a option>
    let opt pa = pa |>> Some <|> returnP None
    
    /// (.>>) :: Parser<'a> -> Parser<'b> -> Parser<'a>
    let (.>>) pa pb = pa .>>. pb |>> fst
    
    /// (>>.) :: Parser<'a> -> Parser<'b> -> Parser<'b>
    let (>>.) pa pb = pa .>>. pb |>> snd
    
    /// between :: Parser<'a> -> Parser<'b> -> Parser<'c> -> Parser<'b>
    let between pa pb pc = pa >>. pb .>> pc
    
    /// sepBy :: Parser<'a> -> Parser<'b> -> Parser<'a list>
    let sepBy1 pa pb = pa .>>. many (pb >>. pa) |>> (fun (a, as') -> a :: as')
    
    /// sepBy :: Parser<'a> -> Parser<'b> -> Parser<'a list>
    let sepBy pa pb = (sepBy1 pa pb) <|> returnP []
