namespace Lib.CPU

module Disassembler = 
    open YaFunTK
    open Lib
    open Lib.Domain.InstructionSet
    open Lib.Parser.Combinators
    open Lib.Parser.Core
        
    let private modRmIndexMap = 
        [| [| MrmTBXSI; MrmTBXSI; MrmTBXSI |]
           [| MrmTBXDI; MrmTBXDI; MrmTBXDI |]
           [| MrmTBPSI; MrmTBPSI; MrmTBPSI |]
           [| MrmTBPDI; MrmTBPDI; MrmTBPDI |]
           [| MrmTSI; MrmTSI; MrmTSI |]
           [| MrmTDI; MrmTDI; MrmTDI |]
           [| MrmTDisp; MrmTBP; MrmTBP |]
           [| MrmTBX; MrmTBX; MrmTBX |] |]
        |> array2D
    
    let inline private getModRmType mo rm = modRmIndexMap.[(int) rm, (int) mo]
    
    let private aocRegMap = 
        [| [| ArgRegister8 AL
              ArgRegister16 AX
              ArgRegister16 AX |]
           [| ArgRegister8 CL
              ArgRegister16 CX
              ArgRegister16 CX |]
           [| ArgRegister8 DL
              ArgRegister16 DX
              ArgRegister16 AX |]
           [| ArgRegister8 BL
              ArgRegister16 BX
              ArgRegister16 BX |]
           [| ArgRegister8 AH
              ArgRegister16 SP
              ArgRegister16 SP |]
           [| ArgRegister8 CH
              ArgRegister16 BP
              ArgRegister16 BP |]
           [| ArgRegister8 DH
              ArgRegister16 SI
              ArgRegister16 SI |]
           [| ArgRegister8 BH
              ArgRegister16 DI
              ArgRegister16 DI |] |]
        |> array2D
    
    let private modRegMap = 
        [| ES
           CS
           SS
           DS |]
    
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
            ((w8 >>> 3) &&& 0b111uy)
            |> returnM
        
        let parseRmArgs w8 = 
            let parseMrm w8 = (w8 >>> 6, w8 &&& 0b111uy)
            
            let parseRmArgs (mo, rm) = 
                let createRmaDeref f d = 
                    (getModRmType mo rm, d |> f |> Some) |> RmaDeref
                match (mo, rm) with
                | (0b00uy, 0b110uy) -> pword16 |>> createRmaDeref W16
                | (0b00uy, _) -> 
                    returnM (RmaDeref (getModRmType mo rm, None))
                | (0b01uy, _) -> pword8 |>> createRmaDeref W8
                | (0b10uy, _) -> pword16 |>> createRmaDeref W16
                | (0b11uy, _) -> returnM (RmaReg(rm))
                | _ -> failwithf "MOD value = %A is unexpected." mo
            w8
            |> parseMrm
            |> parseRmArgs
        
        let parseUseSS w8 = 
            match (w8 >>> 6, w8 &&& 0b111uy) with
            | (0uy, 2uy) | (0uy, 3uy) -> true
            | (1uy, 2uy) | (1uy, 3uy) | (1uy, 6uy) -> true
            | (2uy, 2uy) | (2uy, 3uy) | (2uy, 6uy) -> true
            | _ -> false
            |> returnM
        
        let createpModRegRm reg rm usess = 
            returnM { ModRM = rm
                      MRReg = reg
                      MRUseSS = usess }
        
        pword8
        >>= (fun w8 -> createpModRegRm <!> parseReg w8 <*> parseRmArgs w8 <*> parseUseSS w8)
        >>= id
        <@> "ModRegRM"
    
    let pargument gra (args, mrm) = 
        let getRegister aoc r = 
            let x = (int) r
            let y = ((int) aoc >>> 4) - 1
            aocRegMap.[x, y]
        
        let getDeref (t, d) = 
            function
            | NormalArgCode.B -> ArgDereference8 { DrefType8 = t; DrefDisp8 = d } 
            | NormalArgCode.W | NormalArgCode.P -> ArgDereference16 { DrefType16 = t; DrefDisp16 = d } 
            | _ -> Prelude.undefined
        
        let withMrm a = a, mrm
        let appendArg a = a :: args
        let cpmodRegRm = mrm |> Option.fold (fun _ e -> (returnM e) <@> "cached MRM") (pmodRegRm <@> "raw MRM")
        
        let parseDrefOrReg aoc = 
            cpmodRegRm |>> (fun mrm -> 
            match mrm.ModRM with
            | RmaReg r -> getRegister aoc r |> appendArg, Some mrm
            | RmaDeref d -> getDeref d aoc |> appendArg, Some mrm)
        
        let parseImmediate8 = W8
                              >> ArgImmediate
                              <!> pword8
        let parseImmediate16 = W16
                               >> ArgImmediate
                               <!> pword16

        let parseNormalArgs nac = 
            match nac with
            | (* Ap *) nac when nac = (NormalArgCode.A ||| NormalArgCode.P) -> 
                pword16 .>>. pword16 |>> (fun (o, s) -> 
                { Offset = o
                  Segment = s }
                |> ArgAddress) |>> appendArg |>> withMrm
            | (* Eb *) nac when nac = (NormalArgCode.E ||| NormalArgCode.B) -> parseDrefOrReg NormalArgCode.B
            | (* Ew *) nac when nac = (NormalArgCode.E ||| NormalArgCode.W) -> parseDrefOrReg NormalArgCode.W
            | (* Gb *) nac when nac = (NormalArgCode.G ||| NormalArgCode.B) -> 
                cpmodRegRm |>> (fun mrm -> getRegister NormalArgCode.B mrm.MRReg |> appendArg, Some mrm)
            | (* Gw *) nac when nac = (NormalArgCode.G ||| NormalArgCode.W) -> 
                cpmodRegRm |>> (fun mrm -> getRegister NormalArgCode.W mrm.MRReg |> appendArg, Some mrm)
            | (* I0 *) nac when nac = (NormalArgCode.I ||| NormalArgCode.Z) -> parseImmediate8 |>> appendArg |>> withMrm
            | (* Ib *) nac when nac = (NormalArgCode.I ||| NormalArgCode.B) -> parseImmediate8 |>> appendArg |>> withMrm
            | (* Iw *) nac when nac = (NormalArgCode.I ||| NormalArgCode.W) -> parseImmediate16 |>> appendArg |>> withMrm
            | (* Jb *) nac when nac = (NormalArgCode.J ||| NormalArgCode.B) -> Common.signExtend
                                                                                >> ArgOffset
                                                                                <!> pword8
                                                                                |>> appendArg
                                                                                |>> withMrm
            | (* Jw *) nac when nac = (NormalArgCode.J ||| NormalArgCode.W) -> ArgOffset <!> pword16 |>> appendArg |>> withMrm
            | (* Mp *) nac when nac = (NormalArgCode.M ||| NormalArgCode.P) -> parseDrefOrReg NormalArgCode.P
            | (* Ob *) nac when nac = (NormalArgCode.O ||| NormalArgCode.B) -> 
                pword16 |>> (fun w16 -> 
                ArgDereference8 { DrefType8 = MrmTDisp; DrefDisp8 = w16 |> W16 |> Some }) |>> appendArg |>> withMrm
            | (* Ow *) nac when nac = (NormalArgCode.O ||| NormalArgCode.W) -> 
                pword16 |>> (fun w16 -> 
                ArgDereference16 { DrefType16 = MrmTDisp; DrefDisp16 = w16 |> W16 |> Some }) |>> appendArg |>> withMrm
            | (* Sw *) nac when nac = (NormalArgCode.S ||| NormalArgCode.W) -> 
                cpmodRegRm |>> (fun mrm -> 
                modRegMap.[(int) mrm.MRReg]
                |> ArgRegisterSeg
                |> appendArg, Some mrm)
            | _ -> failwithf "DESC value = %A is unexpected. NAC = %A." gra nac

        
        let parseSpecialArgs = 
            function 
            | SpecialArgCode.AX -> ArgRegister16 AX
            | SpecialArgCode.BX -> ArgRegister16 BX
            | SpecialArgCode.CX -> ArgRegister16 CX
            | SpecialArgCode.DX -> ArgRegister16 DX
            | SpecialArgCode.SP -> ArgRegister16 SP
            | SpecialArgCode.BP -> ArgRegister16 BP
            | SpecialArgCode.SI -> ArgRegister16 SI
            | SpecialArgCode.DI -> ArgRegister16 DI
            | SpecialArgCode.CS -> ArgRegisterSeg CS
            | SpecialArgCode.DS -> ArgRegisterSeg DS
            | SpecialArgCode.ES -> ArgRegisterSeg ES
            | SpecialArgCode.SS -> ArgRegisterSeg SS
            | SpecialArgCode.AL -> ArgRegister8 AL
            | SpecialArgCode.BL -> ArgRegister8 BL
            | SpecialArgCode.CL -> ArgRegister8 CL
            | SpecialArgCode.DL -> ArgRegister8 DL
            | SpecialArgCode.AH -> ArgRegister8 AH
            | SpecialArgCode.BH -> ArgRegister8 BH
            | SpecialArgCode.CH -> ArgRegister8 CH
            | SpecialArgCode.DH -> ArgRegister8 DH
            | SpecialArgCode.One -> 1uy |> ArgConstant
            | SpecialArgCode.Three -> 3uy |> ArgConstant
            | SpecialArgCode.Mem -> failwithf "DESC value = %A is unexpected." gra
        
        match gra with
        | OcaNormal nac -> nac |> parseNormalArgs
        | OcaSpecial sac -> 
            sac
            |> parseSpecialArgs
            |> returnM
            |>> appendArg
            |>> withMrm
            <@> "Argument"
    
    let popCode g = 
        let parseOc = pword8 |>> (fun w8 -> g.OpcRules.[(int) w8])
        
        let getOcg gr mrm = 
            let gr' = g.OpcgRules.[(int)mrm.MRReg, gr.OcId]
            { gr' with OcArgs = 
                           if Array.isEmpty gr'.OcArgs then gr.OcArgs
                           else gr'.OcArgs }, Some mrm
        
        let parseOcg gr = 
            match gr.OcType with
            | OctNormal -> (gr, None) |> returnM
            | OctExtension -> pmodRegRm |>> getOcg gr
        
        parseOc
        >>= parseOcg
        <@> "OpCode"

    // TODO: PERF: Get away from Prelude.Tuple
    
    let pinstruction csip g = 
        let parseAddress = returnM csip
        
        let parseMneumonicAndArgs = 
            let parseMAndAs (gr, mrm) = 
                let parseArgs = 
                    match gr.OcArgs with
                    | [||] -> ([], mrm) |> returnM
                    | [| gra0 |] -> pargument gra0 ([], mrm)
                    | [| gra0; gra1 |] -> 
                        pargument gra0 ([], mrm)
                        >>= (fun am -> pargument gra1 am)
                        >>= (fun (a, m) -> (List.rev a, m) |> returnM)
                    | _ -> Prelude.undefined
                Prelude.tuple2 <!> (returnM gr.OcId) <*> parseArgs
            g
            |> popCode
            >>= parseMAndAs
        
        let createInstruction a (m, (args, mrrm)) bs = 
            { Address = a
              OpCode = m
              UseSS = mrrm |> Option.fold (fun _ e -> e.MRUseSS) false
              Args = args
              Bytes = bs }
        
        (getPosition >>= (fun p -> setUserState (fun _ -> p))) 
        >>. (createInstruction <!> parseAddress <*> parseMneumonicAndArgs 
             <*> (getUserState .>>. getPosition >>= (fun (s, e) -> getInputChunk s e))) <@> "Instruction"
