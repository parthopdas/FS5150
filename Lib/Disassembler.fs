namespace Lib

open Lib.Parser.Combinators
open Lib.Parser.Core

module Disassembler = 
    open Lib.Domain
    
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
        [ (MregT0, 0)
          (MregT1, 1)
          (MregT2, 2)
          (MregT3, 3)
          (MregT4, 4)
          (MregT5, 5)
          (MregT6, 6)
          (MregT7, 7) ]
        |> Map.ofList
    
    /// puint8 :: Parser<Word8>
    let pword8 = satisfy (fun _ -> true) "any byte"
    
    /// pword16 :: Parser<Word16>
    let pword16 = pword8 .>>. pword8 |>> (fun (a, b) -> ((uint64) b <<< 8) + (uint64) a |> uint16)
    
    /// pword32 :: Parser<Word32>
    let pword32 = pword16 .>>. pword16 |>> (fun (a, b) -> ((uint64) b <<< 16) + (uint64) a |> uint32)
    
    /// pmodRegRm :: Parser<ModRegRM>
    let pmodRegRm = 
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
/// popCode :: InstructionSet -> Parser<string * string[]>
/// pargument :: Parser<Argument>
/// pinstruction :: Parser<Instruction>
