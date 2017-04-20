namespace Lib.Chips.I8088

module InstructionSet = 
    open System
    
    type Word8 = uint8
    
    let inline Word8<'T when 'T : (static member op_Explicit : 'T -> uint8)> (value: 'T)  = 
        uint8<'T> value
    
    type Word16 = uint16

    let inline Word16<'T when 'T : (static member op_Explicit : 'T -> uint16)> (value: 'T)  = 
        uint16<'T> value

    type Word32 = uint32

    let inline Word32<'T when 'T : (static member op_Explicit : 'T -> uint32)> (value: 'T)  = 
        uint32<'T> value
    
    type OpCodeDesc = 
        { OcName : string
          OcArgs : string [] }
    
    type OpCodeMap = Map<Word8, OpCodeDesc>
    
    type OpCodeGroup = 
        { OcgName : string
          OcgIndex : Word8 }
    
    type OpCodeExtensioMap = Map<OpCodeGroup, OpCodeDesc>
    
    type InstructionSet = 
        { OpCodes : OpCodeMap
          OpCodeGroups : OpCodeExtensioMap }
    
    [<FlagsAttribute>]
    type NormalArgCode = 
        | None = 0x00
        // Argument Addressing Codes
        | A = 0x01
        | E = 0x02
        | G = 0x03
        | I = 0x04
        | J = 0x05
        | M = 0x06
        | O = 0x07
        | S = 0x08
        | ACMask = 0x0F
        // Argument Operand Codes
        | B = 0x10
        | W = 0x20
        | P = 0x30
        | Z = 0x40
        | OCMask = 0xF0
    
    type SpecialArgCode = 
        | AX
        | BX
        | CX
        | DX
        | SP
        | BP
        | SI
        | DI
        | CS
        | DS
        | ES
        | SS
        | AL
        | BL
        | CL
        | DL
        | AH
        | BH
        | CH
        | DH
        | One
        | Three
        | Mem
    
    type OcArgs = 
        | OcaNormal of NormalArgCode
        | OcaSpecial of SpecialArgCode
    
    type OpCodeType = 
        | OctNormal
        | OctExtension
    
    type GrammerRule = 
        { OcType : OpCodeType
          OcId : int
          OcArgs : OcArgs [] }
    
    type Grammer = 
        { OpcRules : GrammerRule []
          OpcgRules : GrammerRule [,] }
    
    type Address = 
        { Segment : Word16
          Offset : Word16 }
        override x.ToString() = sprintf "%04X:%04X" x.Segment x.Offset
    
    type MachineCode = 
        { Bytes : Word8 array }
    
    type Mneumonic = string
    
    type Constant = Word8
    
    type WordData = 
        | W8 of Word8
        | W16 of Word16
        override x.ToString() = 
            match x with
            | W8 w8 -> sprintf "%02X" w8
            | W16 w16 -> sprintf "%04X" w16
    
    type ModRegType = 
        | MregT0
        | MregT1
        | MregT2
        | MregT3
        | MregT4
        | MregT5
        | MregT6
        | MregT7
    
    type ModRmType = 
        | MrmTBXSI
        | MrmTBXDI
        | MrmTBPSI
        | MrmTBPDI
        | MrmTSI
        | MrmTDI
        | MrmTDisp
        | MrmTBX
        | MrmTBP
        override x.ToString() = 
            match x with
            | MrmTBXSI -> "BX+SI"
            | MrmTBXDI -> "BX+DI"
            | MrmTBPSI -> "BP+SI"
            | MrmTBPDI -> "BP+DI"
            | MrmTSI -> "SI"
            | MrmTDI -> "DI"
            | MrmTDisp -> ""
            | MrmTBX -> "BX"
            | MrmTBP -> "BP"
    
    type DereferencePtr = ModRmType * WordData option

    type Dereference8 =
        { DrefType8 : ModRmType
          DrefDisp8 : WordData option }
        override x.ToString() = 
            match x.DrefDisp8 with
            | Some dval -> 
                match x.DrefType8 with
                | MrmTDisp -> sprintf "byte ptr [%O]" dval
                | _ -> sprintf "byte ptr [%O+%O]" x.DrefType8 dval
            | None -> sprintf "byte ptr [%O]" x.DrefType8

    type Dereference16 =
        { DrefType16 : ModRmType
          DrefDisp16 : WordData option }
        override x.ToString() = 
            match x.DrefDisp16 with
            | Some dval -> 
                match x.DrefType16 with
                | MrmTDisp -> sprintf "word ptr [%O]" dval
                | _ -> sprintf "word ptr [%O+%O]" x.DrefType16 dval
            | None -> sprintf "word ptr [%O]" x.DrefType16
    
    type RmArgs = 
        | RmaReg of Word8
        | RmaDeref of DereferencePtr
    
    type ModRegRM = 
        { ModRM : RmArgs
          MRReg : Word8
          MRUseSS : bool }
    
    type RegisterSeg = 
        | CS
        | DS
        | ES
        | SS
    
    type Register8 = 
        | AL
        | BL
        | CL
        | DL
        | AH
        | BH
        | CH
        | DH
    
    type Register16 = 
        | AX
        | BX
        | CX
        | DX
        | SP
        | BP
        | SI
        | DI
    
    type Argument = 
        | ArgAddress of Address
        | ArgConstant of Constant
        | ArgOffset of Word16
        | ArgRegister8 of Register8
        | ArgRegister16 of Register16
        | ArgRegisterSeg of RegisterSeg
        | ArgImmediate of WordData
        | ArgDereference8 of Dereference8
        | ArgDereference16 of Dereference16
        override x.ToString() = 
            match x with
            | ArgAddress a -> sprintf "%O" a
            | ArgConstant x -> sprintf "%O" x
            | ArgOffset o -> sprintf "%04X" o
            | ArgRegister8 r -> sprintf "%A" r
            | ArgRegister16 r -> sprintf "%A" r
            | ArgRegisterSeg r -> sprintf "%A" r
            | ArgImmediate i -> sprintf "%O" i
            | ArgDereference8 d -> sprintf "%O" d
            | ArgDereference16 d -> sprintf "%O" d
    
    let ocIndices = 
        [| ("--", 0x00)
           ("AAA", 0x01)
           ("AAD", 0x02)
           ("AAM", 0x03)
           ("AAS", 0x04)
           ("ADC", 0x05)
           ("ADD", 0x06)
           ("AND", 0x07)
           ("CALL", 0x08)
           ("CBW", 0x09)
           ("CLC", 0x0A)
           ("CLD", 0x0B)
           ("CLI", 0x0C)
           ("CMC", 0x0D)
           ("CMP", 0x0E)
           ("CMPSB", 0x0F)
           ("CMPSW", 0x10)
           ("CS:", 0x11)
           ("CWD", 0x12)
           ("DAA", 0x13)
           ("DAS", 0x14)
           ("DEC", 0x15)
           ("DIV", 0x16)
           ("DS:", 0x17)
           ("ES:", 0x18)
           ("HLT", 0x19)
           ("IDIV", 0x1A)
           ("IMUL", 0x1B)
           ("IN", 0x1C)
           ("INC", 0x1D)
           ("INT", 0x1E)
           ("INTO", 0x1F)
           ("IRET", 0x20)
           ("JA", 0x21)
           ("JB", 0x22)
           ("JBE", 0x23)
           ("JCXZ", 0x24)
           ("JG", 0x25)
           ("JGE", 0x26)
           ("JL", 0x27)
           ("JLE", 0x28)
           ("JMP", 0x29)
           ("JNB", 0x2A)
           ("JNO", 0x2B)
           ("JNS", 0x2C)
           ("JNZ", 0x2D)
           ("JO", 0x2E)
           ("JPE", 0x2F)
           ("JPO", 0x30)
           ("JS", 0x31)
           ("JZ", 0x32)
           ("LAHF", 0x33)
           ("LDS", 0x34)
           ("LEA", 0x35)
           ("LES", 0x36)
           ("LOCK", 0x37)
           ("LODSB", 0x38)
           ("LODSW", 0x39)
           ("LOOP", 0x3A)
           ("LOOPNZ", 0x3B)
           ("LOOPZ", 0x3C)
           ("MOV", 0x3D)
           ("MOVSB", 0x3E)
           ("MOVSW", 0x3F)
           ("MUL", 0x40)
           ("NEG", 0x41)
           ("NOP", 0x42)
           ("NOT", 0x43)
           ("OR", 0x44)
           ("OUT", 0x45)
           ("POP", 0x46)
           ("POPF", 0x47)
           ("PUSH", 0x48)
           ("PUSHF", 0x49)
           ("RCL", 0x4A)
           ("RCR", 0x4B)
           ("REPNZ", 0x4C)
           ("REPZ", 0x4D)
           ("RET", 0x4E)
           ("RETF", 0x4F)
           ("ROL", 0x50)
           ("ROR", 0x51)
           ("SAHF", 0x52)
           ("SAR", 0x53)
           ("SBB", 0x54)
           ("SCASB", 0x55)
           ("SCASW", 0x56)
           ("SHL", 0x57)
           ("SHR", 0x58)
           ("SS:", 0x59)
           ("STC", 0x5A)
           ("STD", 0x5B)
           ("STI", 0x5C)
           ("STOSB", 0x5D)
           ("STOSW", 0x5E)
           ("SUB", 0x5F)
           ("TEST", 0x60)
           ("WAIT", 0x61)
           ("XCHG", 0x62)
           ("XLAT", 0x63)
           ("XOR", 0x64) |]
        |> Array.map fst
    
    let ocgIndices = 
        [| ("GRP1", 0x0)
           ("GRP2", 0x1)
           ("GRP3a", 0x2)
           ("GRP3b", 0x3)
           ("GRP4", 0x4)
           ("GRP5", 0x5) |]
        |> Array.map fst
    
    type Instruction = 
        { Address : Address
          OpCode : int
          UseSS : bool
          Args : Argument list
          Bytes : Word8 [] }
        
        override x.ToString() = 
            let fmtBytes = 
                sprintf "%s%s" (String.Join("", x.Bytes |> Array.map (sprintf "%02X"))) 
                    (String(' ', 2 * (6 - x.Bytes.Length)))
            
            let fmtArgs = 
                (String.Join(", ", 
                             x.Args
                             |> List.map (fun e -> e.ToString())
                             |> Array.ofList))
            sprintf "%O %s %s%s %O" x.Address fmtBytes ocIndices.[x.OpCode] (if x.IsPrefix then "*"
                                                                              else "") fmtArgs
        
        member x.Length : Word16 = uint16 x.Bytes.Length
        // TODO: PERF: Consider indexing into an array of OpCode descriptor bits
        member x.IsPrefix = 
            x.OpCode = 0x11 || x.OpCode = 0x17 || x.OpCode = 0x18 || x.OpCode = 0x59 || x.OpCode = 0x4C 
            || x.OpCode = 0x4D
    
    type RepetitionType = 
        | NoRepetition
        | WhileZero
        | WhileNotZero
