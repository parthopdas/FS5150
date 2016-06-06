namespace Lib.CPU

module Disassembler = 
    open FSharpx.Functional
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
                    { DrefType = getModRmType mo rm
                      DrefDisp = 
                          d
                          |> f
                          |> Some }
                    |> RmaDeref
                match (mo, rm) with
                | (0b00uy, 0b110uy) -> pword16 |>> createRmaDeref W16
                | (0b00uy, _) -> 
                    returnM (RmaDeref { DrefType = getModRmType mo rm
                                        DrefDisp = None })
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
        
        let createpModRegRm (reg, rm, usess) = 
            returnM { ModRM = rm
                      MRReg = reg
                      MRUseSS = usess }
        
        pword8
        >>= (fun w8 -> Prelude.tuple3 <!> parseReg w8 <*> parseRmArgs w8 <*> parseUseSS w8)
        >>= createpModRegRm
        <@> "ModRegRM"
    
    let pargument gra (args, mrm) = 
        let getRegister aoc r = 
            let x = (int) r
            let y = ((int) aoc >>> 4) - 1
            aocRegMap.[x, y]
        
        let withMrm a = a, mrm
        let appendArg a = a :: args
        let cpmodRegRm = mrm |> Option.fold (fun _ e -> (returnM e) <@> "cached MRM") (pmodRegRm <@> "raw MRM")
        
        let parseDrefOrReg oc = 
            cpmodRegRm |>> (fun mrm -> 
            match mrm.ModRM with
            | RmaReg r -> getRegister oc r |> appendArg, Some mrm
            | RmaDeref d -> ArgDereference d |> appendArg, Some mrm)
        
        let parseImmediate8 = W8
                              >> ArgImmediate
                              <!> pword8
        let parseImmediate16 = W16
                               >> ArgImmediate
                               <!> pword16
        
        let parseNormalArgs nac = 
            let getAddrCode nac = nac &&& NormalArgCode.ACMask
            let getOperandCode nac = nac &&& NormalArgCode.OCMask
            match nac with
            | nac when NormalArgCode.E = getAddrCode nac -> parseDrefOrReg (getOperandCode nac)
            | nac when NormalArgCode.G = getAddrCode nac -> 
                cpmodRegRm |>> (fun mrm -> getRegister (getOperandCode nac) ((uint8) mrm.MRReg) |> appendArg, Some mrm)
            | nac when nac = (NormalArgCode.J ||| NormalArgCode.B) -> Common.signExtend
                                                                      >> ArgOffset
                                                                      <!> pword8
                                                                      |>> appendArg
                                                                      |>> withMrm
            | nac when nac = (NormalArgCode.J ||| NormalArgCode.W) -> ArgOffset <!> pword16 |>> appendArg |>> withMrm
            | nac when nac = (NormalArgCode.I ||| NormalArgCode.B) -> parseImmediate8 |>> appendArg |>> withMrm
            | nac when nac = (NormalArgCode.I ||| NormalArgCode.W) -> parseImmediate16 |>> appendArg |>> withMrm
            | nac when nac = (NormalArgCode.I ||| NormalArgCode.Z) -> parseImmediate8 |>> appendArg |>> withMrm
            | nac when NormalArgCode.O = getAddrCode nac -> 
                pword16 |>> (fun w16 -> 
                ArgDereference { DrefType = MrmTDisp
                                 DrefDisp = 
                                     w16
                                     |> W16
                                     |> Some }) |>> appendArg |>> withMrm
            | nac when nac = (NormalArgCode.S ||| NormalArgCode.W) -> 
                cpmodRegRm |>> (fun mrm -> 
                modRegMap.[(int)mrm.MRReg]
                |> ArgRegisterSeg
                |> appendArg, Some mrm)
            | nac when nac = (NormalArgCode.A ||| NormalArgCode.P) -> 
                pword16 .>>. pword16 |>> (fun (o, s) -> 
                { Offset = o
                  Segment = s }
                |> ArgAddress) |>> appendArg |>> withMrm
            | nac when nac = (NormalArgCode.M ||| NormalArgCode.P) -> parseDrefOrReg NormalArgCode.P
            | _ -> failwithf "DESC value = %A is unexpected." gra
        
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
