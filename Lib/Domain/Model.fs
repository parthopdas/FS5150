namespace Lib.Domain

open System

type Word8 = uint8

type Word16 = uint16

type Word32 = uint32

type OpCodeMap = Map<Word8, string list>

type OpCodeGroup = 
    { OcgName : string
      OcgIndex : Word8 }

type OpCodeExtensioMap = Map<OpCodeGroup, string list>

type InstructionSet = 
    { OpCodes : OpCodeMap
      OpCodeGroups : OpCodeExtensioMap }

type Address = 
    { Segment : Word16
      Offset : Word16 }
    override x.ToString() = sprintf "%04X:%04X" x.Segment x.Offset

type MachineCode = 
    { Bytes : Word8 array }

type Mneumonic = 
    | Mneumonic of string
    override x.ToString() = 
        match x with
        | Mneumonic m -> sprintf "%s" m

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

type Dereference = 
    { DrefType : ModRmType
      DrefDisp : WordData option }
    override x.ToString() = 
        match x.DrefDisp with
        | Some dval -> 
            match x.DrefType with
            | MrmTDisp -> sprintf "[%O]" dval
            | _ -> sprintf "[%O+%O]" x.DrefType dval
        | None -> sprintf "[%O]" x.DrefType

type RmArgs = 
    | RmaReg of ModRegType
    | RmaDeref of Dereference

type ModRegRM = 
    { ModReg : ModRegType
      ModRM : RmArgs }

type Register = 
    | AL
    | BL
    | CL
    | DL
    | AH
    | BH
    | CH
    | DH
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
    | IP

type Argument = 
    | ArgAddress of Address
    | ArgConstant of Constant
    | ArgOffset of WordData
    | ArgRegister of Register
    | ArgImmediate of WordData
    | ArgDereference of Dereference
    override x.ToString() = 
        match x with
        | ArgAddress a -> sprintf "%O" a
        | ArgConstant x -> sprintf "%O" x
        | ArgOffset o -> sprintf "%O" o
        | ArgRegister r -> sprintf "%A" r
        | ArgImmediate i -> sprintf "%O" i
        | ArgDereference d -> sprintf "%O" d

type Instruction = 
    { Address : Address
      Mneumonic : Mneumonic
      Args : Argument list
      Bytes : Word8[] }
    override x.ToString() = 
        let fmtBytes = sprintf "%s%s" (String.Join("", x.Bytes |> Array.map (sprintf "%02X"))) (new String(' ', 2 * (6 - x.Bytes.Length)))
        let fmtArgs = (String.Join(", ", x.Args |> List.map (fun e -> e.ToString()) |> Array.ofList))
        sprintf "%O %s %O\t%O" 
            x.Address fmtBytes x.Mneumonic fmtArgs
