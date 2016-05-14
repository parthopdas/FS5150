namespace Lib

module Disassembler = 
    open FSharpx.Text
    open Lib.Domain.InstructionSet
    open Lib.Parser.Combinators
    open Lib.Parser.Core
    open Microsoft.FSharp.Reflection
    
    let private fromString<'a> (s : string) = 
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None
    
    let private modRegIndexMap = 
        [ (0b000uy, MregT0)
          (0b001uy, MregT1)
          (0b010uy, MregT2)
          (0b011uy, MregT3)
          (0b100uy, MregT4)
          (0b101uy, MregT5)
          (0b110uy, MregT6)
          (0b111uy, MregT7) ]
        |> Map.ofList
    
    let private getModRegType r = modRegIndexMap.[r]
    
    let private modRmIndexMap = 
        [ ((0b000uy, 0b00uy), MrmTBXSI)
          ((0b000uy, 0b01uy), MrmTBXSI)
          ((0b000uy, 0b10uy), MrmTBXSI)
          ((0b001uy, 0b00uy), MrmTBXDI)
          ((0b001uy, 0b01uy), MrmTBXDI)
          ((0b001uy, 0b10uy), MrmTBXDI)
          ((0b010uy, 0b00uy), MrmTBPSI)
          ((0b010uy, 0b01uy), MrmTBPSI)
          ((0b010uy, 0b10uy), MrmTBPSI)
          ((0b011uy, 0b00uy), MrmTBPDI)
          ((0b011uy, 0b01uy), MrmTBPDI)
          ((0b011uy, 0b10uy), MrmTBPDI)
          ((0b100uy, 0b00uy), MrmTSI)
          ((0b100uy, 0b01uy), MrmTSI)
          ((0b100uy, 0b10uy), MrmTSI)
          ((0b101uy, 0b00uy), MrmTDI)
          ((0b101uy, 0b01uy), MrmTDI)
          ((0b101uy, 0b10uy), MrmTDI)
          ((0b110uy, 0b00uy), MrmTDisp)
          ((0b110uy, 0b01uy), MrmTBP)
          ((0b110uy, 0b10uy), MrmTBP)
          ((0b111uy, 0b00uy), MrmTBX)
          ((0b111uy, 0b01uy), MrmTBX)
          ((0b111uy, 0b10uy), MrmTBX) ]
        |> Map.ofList
    
    let private getModRmType mo rm = modRmIndexMap.[(rm, mo)]
    
    let private modRegOcgIndex = 
        [ (MregT0, 0uy)
          (MregT1, 1uy)
          (MregT2, 2uy)
          (MregT3, 3uy)
          (MregT4, 4uy)
          (MregT5, 5uy)
          (MregT6, 6uy)
          (MregT7, 7uy) ]
        |> Map.ofList
    
    let private descRegMap = 
        [ ("eAX", AX)
          ("eCX", CX)
          ("eBX", BX)
          ("eDX", DX)
          ("eSP", SP)
          ("eBP", BP)
          ("eSI", SI)
          ("eDI", DI)
          ("AX", AX)
          ("BX", BX)
          ("CX", CX)
          ("DX", DX)
          ("SP", SP)
          ("BP", BP)
          ("SI", SI)
          ("DI", DI)
          ("CS", CS)
          ("DS", DS)
          ("ES", ES)
          ("SS", SS)
          ("AL", AL)
          ("BL", BL)
          ("CL", CL)
          ("DL", DL)
          ("AH", AH)
          ("BH", BH)
          ("CH", CH)
          ("DH", DH) ]
        |> Map.ofList
    
    let private aocRegMap = 
        [ (('b', MregT0), AL)
          (('b', MregT1), CL)
          (('b', MregT2), DL)
          (('b', MregT3), BL)
          (('b', MregT4), AH)
          (('b', MregT5), CH)
          (('b', MregT6), DH)
          (('b', MregT7), BH)
          (('v', MregT0), AX)
          (('v', MregT1), CX)
          (('v', MregT2), DX)
          (('v', MregT3), BX)
          (('v', MregT4), SP)
          (('v', MregT5), BP)
          (('v', MregT6), SI)
          (('v', MregT7), DI)
          (('w', MregT0), AX)
          (('w', MregT1), CX)
          (('w', MregT2), DX)
          (('w', MregT3), BX)
          (('w', MregT4), SP)
          (('w', MregT5), BP)
          (('w', MregT6), SI)
          (('w', MregT7), DI)
          (('p', MregT0), AX)
          (('p', MregT1), CX)
          (('p', MregT2), DX)
          (('p', MregT3), BX)
          (('p', MregT4), SP)
          (('p', MregT5), BP)
          (('p', MregT6), SI)
          (('p', MregT7), DI) ]
        |> Map.ofList
    
    let private modRegMap = 
        [ (MregT0, ES)
          (MregT1, CS)
          (MregT2, SS)
          (MregT3, DS) ]
        |> Map.ofList
    
    /// puint8 :: Parser<Word8>
    let pword8<'a> : Parser<Word8, 'a> = satisfy (fun _ -> true) "word8" <@> "word8"
    
    /// pword16 :: Parser<Word16>
    let pword16<'a> : Parser<Word16, 'a> = 
        pword8 .>>. pword8 |>> (fun (a, b) -> ((uint64) b <<< 8) + (uint64) a |> uint16) <@> "word16"
    
    /// pword32 :: Parser<Word32>
    let pword32<'a> : Parser<Word32, 'a> = 
        pword16 .>>. pword16 |>> (fun (a, b) -> ((uint64) b <<< 16) + (uint64) a |> uint32) <@> "word32"
    
    /// pmodRegRm :: Parser<ModRegRM>
    let pmodRegRm<'a> : Parser<ModRegRM, 'a> = 
        let parseReg w8 = 
            ((w8 >>> 0b11) &&& 0b111uy)
            |> getModRegType
            |> returnP
        
        let parseRmArgs w8 = 
            let parseMrm w8 = (w8 >>> 0b110, w8 &&& 0b111uy)
            
            let parseRmArgs (mo, rm) = 
                let createRmaDeref f d = 
                    { DrefType = getModRmType mo rm
                      DrefDisp = 
                          d
                          |> f
                          |> Some }
                    |> RmaDeref
                match (mo, rm) with
                | (0b00uy, 0b110uy) -> pword16 |>> createRmaDeref W16
                | (0b00uy, _) -> 
                    returnP (RmaDeref { DrefType = getModRmType mo rm
                                        DrefDisp = None })
                | (0b01uy, _) -> pword8 |>> createRmaDeref W8
                | (0b10uy, _) -> pword16 |>> createRmaDeref W16
                | (0b11uy, _) -> returnP (RmaReg(getModRegType rm))
                | _ -> failwithf "MOD value = %A is unexpected." mo
            w8
            |> parseMrm
            |> parseRmArgs
        
        let createpModRegRm (reg, rm) = 
            returnP { ModReg = reg
                      ModRM = rm }
        
        let f a b = a, b
        pword8
        >>= (fun w8 -> f <!> parseReg w8 <*> parseRmArgs w8)
        >>= createpModRegRm
        <@> "ModRegRM"
    
    /// pargument :: string -> Argument list * ModRegRM opt -> Parser<Argument list * ModRegRM opt>
    let pargument desc (args, mrm) = 
        let getRegister aoc r = aocRegMap.[(aoc, r)] |> ArgRegister
        let withMrm a = a, mrm
        let appendArg a = a :: args
        let cpmodRegRm = mrm |> Option.fold (fun _ e -> (returnP e) <@> "cached MRM") (pmodRegRm <@> "raw MRM")
        
        let parseDrefOrReg aoc = 
            cpmodRegRm |>> (fun mrm -> 
            match mrm.ModRM with
            | RmaReg r -> getRegister aoc r |> appendArg, Some mrm
            | RmaDeref d -> ArgDereference d |> appendArg, Some mrm)
        
        let parseImmediate8 = W8
                              >> ArgImmediate
                              <!> pword8
        let parseImmediate16 = W16
                               >> ArgImmediate
                               <!> pword16
        
        let parseNonRegArgs = 
            function 
            | [ 'E'; aoc ] -> parseDrefOrReg aoc
            | [ 'G'; aoc ] -> 
                cpmodRegRm |>> (fun mrm -> 
                mrm.ModReg
                |> getRegister aoc
                |> appendArg, Some mrm)
            | [ 'J'; 'b' ] -> W8
                              >> ArgOffset
                              <!> pword8
                              |>> appendArg
                              |>> withMrm
            | [ 'J'; 'v' ] -> W16
                              >> ArgOffset
                              <!> pword16
                              |>> appendArg
                              |>> withMrm
            | [ 'I'; 'b' ] -> parseImmediate8 |>> appendArg |>> withMrm
            | [ 'I'; 'v' ] -> parseImmediate16 |>> appendArg |>> withMrm
            | [ 'I'; 'w' ] -> parseImmediate16 |>> appendArg |>> withMrm
            | [ 'I'; '0' ] -> parseImmediate8 |>> appendArg |>> withMrm
            | [ 'O'; _ ] -> 
                pword16 |>> (fun w16 -> 
                ArgDereference { DrefType = MrmTDisp
                                 DrefDisp = 
                                     w16
                                     |> W16
                                     |> Some }) |>> appendArg |>> withMrm
            | [ 'S'; 'w' ] -> 
                cpmodRegRm |>> (fun mrm -> 
                modRegMap.[mrm.ModReg]
                |> ArgRegister
                |> appendArg, Some mrm)
            | [ 'A'; 'p' ] -> 
                pword16 .>>. pword16 |>> (fun (o, s) -> 
                { Offset = o
                  Segment = s }
                |> ArgAddress) |>> appendArg |>> withMrm
            | [ '1' ] -> 
                1uy
                |> ArgConstant
                |> returnP
                |>> appendArg
                |>> withMrm
            | [ '3' ] -> 
                3uy
                |> ArgConstant
                |> returnP
                |>> appendArg
                |>> withMrm
            | [ 'M'; 'p' ] -> parseDrefOrReg 'p'
            | _ -> failwithf "DESC value = %A is unexpected." desc
        (match descRegMap |> Map.tryFind desc with
         | Some r -> 
             r
             |> ArgRegister
             |> appendArg
             |> withMrm
             |> returnP
         | None -> 
             desc
             |> Seq.toList
             |> parseNonRegArgs)
        <@> "Argument"
    
    /// popCode :: InstructionSet -> Parser<string * string[]>
    let popCode is = 
        let getOc w8 = 
            let oc = is.OpCodes.[w8]
            List.head oc, List.tail oc
        
        let getOcg o a mrm = 
            match is.OpCodeGroups.[{ OcgName = o
                                     OcgIndex = modRegOcgIndex.[mrm.ModReg] }] with
            | [] -> failwith "OpCodeGroups has invalid entry"
            | ox :: ax -> 
                if ox = "--" then ("???", [], Some mrm)
                else 
                    (ox, 
                     (if [] = ax then a
                      else ax), Some mrm)
        
        let parseOc = getOc <!> pword8
        
        let parseOcg (o, a) = 
            if (Strings.startsWith "GRP" o) then pmodRegRm |>> getOcg o a
            else (o, a, None) |> returnP
        parseOc
        >>= parseOcg
        <@> "OpCode"
    
    /// pinstruction :: Address -> InstructionSet -> Parser<Instruction>
    let pinstruction csip is = 
        let parseAddress = returnP csip
        
        let parseMneumonicAndArgs = 
            let parseMAndAs (oc, ocas, mrm) = 
                let parseMneumonic oc = 
                    match oc |> fromString<Mneumonic> with
                    | Some m -> m |> returnP
                    | None -> failwithf "Invalid instruction: %s" oc
                
                let parseArgs = (ocas, (([], mrm) |> returnP))
                                ||> List.foldBack (fun e acc -> acc >>= pargument e)
                                |>> fst
                (fun a b -> a, b) <!> (parseMneumonic oc) <*> parseArgs
            is
            |> popCode
            >>= parseMAndAs
        
        let createInstruction a (m, args) bs = 
            { Address = a
              Mneumonic = m
              Args = args
              Bytes = bs }
        
        (getPosition >>= (fun p -> setUserState (fun _ -> p))) 
        >>. (createInstruction <!> parseAddress <*> parseMneumonicAndArgs 
             <*> (getUserState .>>. getPosition >>= (fun (s, e) -> getInputChunk s e))) <@> "Instruction"
