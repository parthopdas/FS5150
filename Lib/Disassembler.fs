namespace Lib

open Lib.Parser.Combinators
open Lib.Parser.Core

module Disassembler = 
    open Lib.Domain
    
    let modRegIndexMap = 
        [ (0b000, MregT0)
          (0b001, MregT1)
          (0b010, MregT2)
          (0b011, MregT3)
          (0b100, MregT4)
          (0b101, MregT5)
          (0b110, MregT6)
          (0b111, MregT7) ]
        |> Map.ofList
    
    let modRmIndexMap = 
        [ ((0b000, 0b00), MrmTBXSI)
          ((0b000, 0b01), MrmTBXSI)
          ((0b000, 0b10), MrmTBXSI)
          ((0b001, 0b00), MrmTBXDI)
          ((0b001, 0b01), MrmTBXDI)
          ((0b001, 0b10), MrmTBXDI)
          ((0b010, 0b00), MrmTBPSI)
          ((0b010, 0b01), MrmTBPSI)
          ((0b010, 0b10), MrmTBPSI)
          ((0b011, 0b00), MrmTBPDI)
          ((0b011, 0b01), MrmTBPDI)
          ((0b011, 0b10), MrmTBPDI)
          ((0b100, 0b00), MrmTSI)
          ((0b100, 0b01), MrmTSI)
          ((0b100, 0b10), MrmTSI)
          ((0b101, 0b00), MrmTDI)
          ((0b101, 0b01), MrmTDI)
          ((0b101, 0b10), MrmTDI)
          ((0b110, 0b00), MrmTDisp)
          ((0b110, 0b01), MrmTBP)
          ((0b110, 0b10), MrmTBP)
          ((0b111, 0b00), MrmTBX)
          ((0b111, 0b01), MrmTBX)
          ((0b111, 0b10), MrmTBX) ]
        |> Map.ofList
    
    let modRegOcgIndex = 
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
/// popCode :: InstructionSet -> Parser<string * string[]>
/// pargument :: Parser<Argument>
/// pinstruction :: Parser<Instruction>
