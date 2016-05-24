namespace Lib.CPU.Execution

module Common = 
    open FSharpx
    open FSharpx.State
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    let inline getHiByte w16 =
        w16 >>> 8 |> uint8 : Word8
    
    let inline getLoByte w16 =
        w16 &&& 0x00FFus |> uint8 : Word8
    
    let inline setHiByte w16 (value : Word8) =
        ((uint16)value <<< 8) + (uint16)(getLoByte w16) : Word16
    
    let inline setLoByte w16 (value : Word8) =
        (uint16)(getHiByte w16) + ((uint16)value) : Word16
    
    /// flatten :: Address -> uint32
    let private flatten addr = 
        ((uint32) addr.Segment <<< 4) + (uint32) addr.Offset
        |> (&&&) 0xFFFFFu
        |> uint32
    
    /// createAddr Word16 -> Word16 -> Address
    let createAddr segment offset = 
        { Segment = segment
          Offset = offset }
    
    /// incrExecedCount :: unit -> State<unit,Motherboard>
    let incrExecedCount () = 
        let innerFn mb = 
            (mb.ExecutedCount <- mb.ExecutedCount + 1), mb
        innerFn : State<unit, Motherboard>
    
    /// incrAddress :: Word16 -> Address -> Address
    let incrAddress n addr = { addr with Address.Offset = ((addr.Offset + n) &&& 0xFFFFus) }
    
    /// readWord8 :: Address -> State<Word8,Motherboard>
    let readWord8 addr = 
        let innerFn mb = 
            match flatten addr with
            | i when i >= 0xFE000u && i < 0x100000u -> mb.BIOS.[(int32) (i - 0xFE000u)], mb
            | i -> mb.RAM.[(int32) i], mb
        innerFn : State<Word8, Motherboard>
    
    /// writeWord8 :: Address -> Word8 -> State<unit,Motherboard>
    let writeWord8 value addr = 
        let innerFn mb = 
            match flatten addr with
            | i when i >= 0xFE000u && i < 0x100000u -> ()
            | i -> mb.RAM.[(int32) i] <- value
            (), mb
        innerFn : State<unit, Motherboard>
    
    /// writeWord16 :: Word8 -> Address -> State<unit,Motherboard>
    let writeWord16 (value : Word16) addr = 
        (writeWord8 (getLoByte value) addr) >>. (writeWord8 (getHiByte value) (incrAddress 1us addr))
    
    /// outPort16to16 :: Word16 -> Word16 -> State<unit,Motherboard>
    let outPort16to16 (pno : Word16) (value : Word16) = 
        let innerFn mb = 
            if pno < 4096us then mb.PortRAM.[(int)pno] <- (uint16)value
            match pno with
            | i when i >= 0x20us && i <= 0x21us -> () //failwith "8259 - Write To Port - NYI"
            | i when i >= 0x40us && i <= 0x43us -> () //failwith "8253 - Write To Port - NYI"
            | i when i = 0x61us -> () //failwith "Speaker - Write To Port - NYI"
            | i when i >= 0x388us && i <= 0x389us -> () //failwith "Adlib pass-through - Write To Port - NYI"
            | i when i >= 0x3C7us && i <= 0x3C9us -> () //failwith "MCGA/VGA DAC read palette index - Write To Port - NYI"
            | i when i = 0x3D4us -> () //failwith "CGA/VGA CRTC register select index - Write To Port - NYI"
            | i when i = 0x3D5us -> () //failwith "CGA/VGA CRTC register data - Write To Port - NYI"
            | i when i = 0x3D9us -> () //failwith "? - Write To Port - NYI"
            | i when i >= 0x3F8us && i <= 0x3FFus -> () //failwith "COM1 - Write To Port - NYI"
            | _ -> ()
            (), mb
        innerFn : State<unit, Motherboard>
    
    /// outPort8to8 :: Word8 -> Word8 -> State<unit,Motherboard>
    let outPort8to8 (pno : Word8) (value : Word8) = 
        outPort16to16 ((uint16)pno) ((uint16)value)
    
    /// outPort8to16 :: Word16 -> Word8 -> State<unit,Motherboard>
    let outPort8to16 (pno : Word16) (value : Word8) = 
        outPort16to16 ((uint16)pno) ((uint16)value)
    
    /// outPort16to8 :: Word8 -> Word16 -> State<unit,Motherboard>
    let outPort16to8 (pno : Word8) (value : Word16) = 
        outPort16to16 ((uint16)pno) ((uint16)value)

    /// getCSIP : State<Address,Motherboard>
    let getCSIP = 
        let innerFn mb = 
            { Segment = mb.CPU.CS
              Offset = mb.CPU.IP }, mb
        innerFn : State<Address, Motherboard>
    
    /// getFlag : flag -> State<bool,Motherboard>
    let getFlag flag = 
        let innerFn mb =
            mb.CPU.Flags.[flag], mb
        innerFn : State<bool, Motherboard>
    
    /// setFlag : flag -> value -> State<unit,Motherboard>
    let setFlag flag value = 
        let innerFn mb =
            mb.CPU.Flags.[flag] <- value
            (), mb
        innerFn : State<unit, Motherboard>
        
    /// setCSIP : State<unit,Motherboard>
    let setCSIP addr = 
        let innerFn mb = 
            mb.CPU.CS <- addr.Segment
            mb.CPU.IP <- addr.Offset
            (), mb
        innerFn : State<unit, Motherboard>

    /// getRegSeg : RegiterSeg -> State<Word16,Motherboard>
    let getRegSeg segReg = 
        let innerFn mb = 
            let data = 
                match segReg with
                | CS -> mb.CPU.CS
                | DS -> mb.CPU.DS
                | ES -> mb.CPU.ES
                | SS -> mb.CPU.SS
            data, mb
        innerFn : State<Word16, Motherboard>
    
    /// setRegSeg : RegiterSeg -> State<unit,Motherboard>
    let setRegSeg segReg value = 
        let innerFn mb = 
            match segReg with
            | CS -> mb.CPU.CS <- value
            | DS -> mb.CPU.DS <- value
            | ES -> mb.CPU.ES <- value
            | SS -> mb.CPU.SS <- value
            (), mb
        innerFn : State<unit, Motherboard>

    /// getReg8 : Regiter -> State<Word8,Motherboard>
    let getReg8 reg = 
        let innerFn mb = 
            let data = 
                match reg with
                | AL -> getLoByte mb.CPU.AX
                | AH -> getHiByte mb.CPU.AX
                | BL -> getLoByte mb.CPU.BX
                | BH -> getHiByte mb.CPU.BX
                | CL -> getLoByte mb.CPU.CX 
                | CH -> getHiByte mb.CPU.CX
                | DL -> getLoByte mb.CPU.DX
                | DH -> getHiByte mb.CPU.DX
            data, mb
        innerFn : State<Word8, Motherboard>
    
    /// setReg8 : Regiter -> Word8 -> State<unit,Motherboard>
    let setReg8 reg value = 
        let innerFn mb = 
            match reg with
            | AL -> mb.CPU.AX <- setLoByte mb.CPU.AX value
            | AH -> mb.CPU.AX <- setHiByte mb.CPU.AX value
            | BL -> mb.CPU.BX <- setLoByte mb.CPU.BX value
            | BH -> mb.CPU.BX <- setHiByte mb.CPU.BX value
            | CL -> mb.CPU.CX <- setLoByte mb.CPU.CX value
            | CH -> mb.CPU.CX <- setHiByte mb.CPU.CX value
            | DL -> mb.CPU.DX <- setLoByte mb.CPU.DX value
            | DH -> mb.CPU.DX <- setHiByte mb.CPU.DX value
            (), mb
        innerFn : State<unit, Motherboard>
    
    /// getReg16 : Regiter -> State<Word16,Motherboard>
    let getReg16 reg = 
        let innerFn mb = 
            let data = 
                match reg with
                | AX -> mb.CPU.AX
                | BX -> mb.CPU.BX
                | CX -> mb.CPU.CX
                | DX -> mb.CPU.DX
                | SP -> mb.CPU.SP
                | BP -> mb.CPU.BP
                | SI -> mb.CPU.SI
                | DI -> mb.CPU.DI
            data, mb
        innerFn : State<Word16, Motherboard>
    
    /// setReg16 : Regiter -> Word16 -> State<unit,Motherboard>
    let setReg16 reg value = 
        let innerFn mb = 
            match reg with
            | AX -> mb.CPU.AX <- value
            | BX -> mb.CPU.BX <- value
            | CX -> mb.CPU.CX <- value
            | DX -> mb.CPU.DX <- value
            | SP -> mb.CPU.SP <- value
            | BP -> mb.CPU.BP <- value
            | SI -> mb.CPU.SI <- value
            | DI -> mb.CPU.DI <- value
            (), mb
        innerFn : State<unit, Motherboard>
    
    /// getEffectiveAddress :: Dereference -> State<Address,Motherboard>
    let getEA deref = 
        let reg = 
            match deref.DrefType with
            | MrmTBXSI -> (+) <!> getReg16 BX <*> getReg16 SI
            | MrmTBXDI -> (+) <!> getReg16 BX <*> getReg16 DI
            | MrmTBPSI -> (+) <!> getReg16 BP <*> getReg16 SI
            | MrmTBPDI -> (+) <!> getReg16 BP <*> getReg16 SI
            | MrmTSI -> getReg16 SI
            | MrmTDI -> getReg16 DI
            | MrmTDisp -> 0us |> State.returnM
            | MrmTBX -> getReg16 BX
            | MrmTBP -> getReg16 BP
        
        let disp = 
            deref.DrefDisp
            |> Option.fold (fun acc e -> 
                   acc + match e with
                         | W8 w8 -> (uint16) w8
                         | W16 w16 -> w16) 0us
            |> State.returnM
        
        (+) <!> reg <*> disp
    
    /// getSegOverrideForEA : ModRegRm option -> State<Word16,Motherboard>
    let getSegOverrideForEA (usess :bool) : State<RegisterSeg,Motherboard> =
        let innerFn mb =
            let sr =
                mb.CPU.SegOverride 
                |> Option.orElse (if usess then Some SS else None)
                |> Option.getOrElse DS
            sr, mb
        innerFn : State<RegisterSeg, Motherboard>

    let failwithnyi instr = failwithf "%O - Not implemented" (instr.ToString())

    let ns<'T> : State<'T option, Motherboard> = None |> State.returnM
    
    let parity = 
        [| true; false; false; true; false; true; true; false; false; true; true; false; true; false; false; true; false; 
           true; true; false; true; false; false; true; true; false; false; true; false; true; true; false; false; true; 
           true; false; true; false; false; true; true; false; false; true; false; true; true; false; true; false; false; 
           true; false; true; true; false; false; true; true; false; true; false; false; true; false; true; true; false; 
           true; false; false; true; true; false; false; true; false; true; true; false; true; false; false; true; false; 
           true; true; false; false; true; true; false; true; false; false; true; true; false; false; true; false; true; 
           true; false; false; true; true; false; true; false; false; true; false; true; true; false; true; false; false; 
           true; true; false; false; true; false; true; true; false; false; true; true; false; true; false; false; true; 
           true; false; false; true; false; true; true; false; true; false; false; true; false; true; true; false; false; 
           true; true; false; true; false; false; true; true; false; false; true; false; true; true; false; false; true; 
           true; false; true; false; false; true; false; true; true; false; true; false; false; true; true; false; false; 
           true; false; true; true; false; true; false; false; true; false; true; true; false; false; true; true; false; 
           true; false; false; true; false; true; true; false; true; false; false; true; true; false; false; true; false; 
           true; true; false; false; true; true; false; true; false; false; true; true; false; false; true; false; true; 
           true; false; true; false; false; true; false; true; true; false; false; true; true; false; true; false; false; 
           true |]

    let flagSZP16 w16 = 
        (setFlag SF (w16 &&& 0x8000us = 0x8000us)) *> (setFlag ZF (w16 = 0us)) 
        *> (setFlag PF (parity.[(int) (w16 &&& 255us)]))
