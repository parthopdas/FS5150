namespace Lib.Domain

type Word8 = uint8

type Word16 = uint16

type Word32 = uint32

type Address = 
    { Segment : Word16
      Offset : Word16 }

//instance Show Address where
//  show (Address seg off) = printf "%04X:%04X" seg off
type MachineCode = 
    { Bytes : Word8 array }

type Mneumonic = 
    { Name : string }

//instance Show Mneumonic where
//  show (Mneumonic n) = printf "%s" n
type Constant = Word8

type WordData = 
    | W8 of Word8
    | W16 of Word16

//instance Show Inttype where
//  show (Int8 d) = printf "%02X" d
//  show (Int16 d) = printf "%04X" d
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

//instance Show ModRmType where
//  show MrmTBXSI = "BX+SI"
//  show MrmTBXDI = "BX+DI"
//  show MrmTBPSI = "BP+SI"
//  show MrmTBPDI = "BP+DI"
//  show MrmTSI = "SI"
//  show MrmTDI = "DI"
//  show MrmTDisp = ""
//  show MrmTBX = "BX"
//  show MrmTBP = "BP"
type Dereference = 
    { DrefType : ModRmType
      DrefDisp : WordData option }

//instance Show Dereference where
//  show (Dereference t d) = case d of
//    Just dval -> printf "[%s+%s]" (show t) (show dval)
//    Nothing -> printf "[%s]" (show t)
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

//instance Show Argument where
//  show (ArgAddress a) = show a
//  show (ArgConstant c) = show c
//  show (ArgOffset o) = show o
//  show (ArgRegister r) = show r
//  show (ArgImmediate i) = show i
//  show (ArgDereference d) = show d
type Instruction = 
    { Address : Address
      Mneumonic : Mneumonic
      Args : Argument array }
//instance Show Instruction where
//  show (Instruction addr mne as) = printf "%s %s\t %s" (show addr) (show mne) (intercalate ", " $ map show as)
