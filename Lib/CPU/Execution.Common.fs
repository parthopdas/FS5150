namespace Lib.CPU.Execution

module Common = 
    open FSharpx
    open FSharpx.Functional
    open FSharpx.State
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    
    (* Byte/Word/Address manipulation *)
    let inline (!<>) (w8 : Word8) : Word16 = (uint16) w8
    let inline (!><) (w16 : Word16) : Word8 = (uint8) w16
    let inline getHiByte w16 : Word8 = w16 >>> 8 |> uint8
    let inline getLoByte w16 : Word8 = w16 &&& 0x00FFus |> uint8
    let inline setHiByte w16 (value : Word8) : Word16 = ((uint16) value <<< 8) + (uint16) (getLoByte w16)
    let inline setLoByte w16 (value : Word8) : Word16 = (uint16) (getHiByte w16) + ((uint16) value)
    let inline makeWord16 (hi : Word8, lo : Word8) : Word16 = !<>hi <<< 8 ||| !<>lo
    
    let inline flatten addr = 
        ((uint32) addr.Segment <<< 4) + (uint32) addr.Offset
        |> (&&&) 0xFFFFFu
        |> uint32
    
    let inline (|++) n addr = { addr with Address.Offset = ((addr.Offset + n) &&& 0xFFFFus) }
    
    let inline createAddr segment offset = 
        { Segment = segment
          Offset = offset }
    
    (* CPU Statistics *)
    let incrExecedCount() = 
        let innerFn mb = (mb.ExecutedCount <- mb.ExecutedCount + 1), mb
        innerFn : State<unit, Motherboard>
    
    (* Register IO *)
    let getFlag flag = 
        let innerFn mb = mb.CPU.Flags.[flag], mb
        innerFn : State<bool, Motherboard>
    
    let setFlag flag value = 
        let innerFn mb = 
            mb.CPU.Flags.[flag] <- value
            (), mb
        innerFn : State<unit, Motherboard>
    
    let setCSIP addr = 
        let innerFn mb = 
            mb.CPU.CS <- addr.Segment
            mb.CPU.IP <- addr.Offset
            (), mb
        innerFn : State<unit, Motherboard>
    
    let getCSIP = 
        let innerFn mb = 
            { Segment = mb.CPU.CS
              Offset = mb.CPU.IP }, mb
        innerFn : State<Address, Motherboard>
    
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
    
    let setRegSeg segReg value = 
        let innerFn mb = 
            match segReg with
            | CS -> mb.CPU.CS <- value
            | DS -> mb.CPU.DS <- value
            | ES -> mb.CPU.ES <- value
            | SS -> mb.CPU.SS <- value
            (), mb
        innerFn : State<unit, Motherboard>
    
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
    
    (* Memory IO *)
    let readWord8 addr = 
        let innerFn mb = 
            let addr = int32 (flatten addr)
            mb.RAM.[addr], mb
        innerFn : State<Word8, Motherboard>
    
    let readWord16 addr = 
        Prelude.tuple2 <!> readWord8 (1us |++ addr) <*> readWord8 addr >>= (makeWord16 >> State.returnM)
    
    let writeWord8 value addr = 
        let innerFn mb = 
            let addr = int32 (flatten addr)
            if not mb.ReadOnly.[addr] then mb.RAM.[addr] <- value
            (), mb
        innerFn : State<unit, Motherboard>
    
    let writeWord16 (value : Word16) addr = 
        (writeWord8 (getLoByte value) addr) >>. (writeWord8 (getHiByte value) (1us |++ addr))

    (* Device IO *)
    let portReadCallbacks : Map<Word16, Word16 -> Word8> = Map.empty
    
    let portRead (pno : Word16) = 
        let innerFn mb = 
            let pval = 
                match pno with
                | 0x62us -> 0x00uy
                | 0x60us | 0x61us | 0x63us | 0x64us -> mb.PortRAM.[(int) pno]
                | _ -> 
                    portReadCallbacks
                    |> Map.tryFind pno
                    |> Option.fold (fun _ e -> e pno) 0xFFuy
            pval, mb
        innerFn : State<Word8, Motherboard>
    
    let portRead16Callbacks : Map<Word16, Word16 -> Word16> = Map.empty
    
    let portRead16 (pno : Word16) = 
        let ifNoCallback = Prelude.tuple2 <!> portRead (pno + 1us) <*> portRead pno >>= (makeWord16 >> State.returnM)
        portRead16Callbacks
        |> Map.tryFind pno
        |> Option.fold (fun _ e -> e pno |> State.returnM) ifNoCallback : State<Word16, Motherboard>
    
    let portWriteCallbacks : Map<Word16, Word16 -> Word8 -> unit> = Map.empty
    
    let portWrite (pno : Word16) (value : Word8) = 
        let innerFn mb = 
            mb.PortRAM.[(int) pno] <- value
            match pno with
            | 0x61us -> printfn "Writing to port 61 (%d)- Enabling/disabing speaker NYI" value
            | _ -> ()
            portWriteCallbacks
            |> Map.tryFind pno
            |> Option.fold (fun _ e -> e pno value) ()
            (), mb
        innerFn : State<unit, Motherboard>
    
    let portWrite16Callbacks : Map<Word16, Word16 -> Word16 -> unit> = Map.empty
    
    let portWrite16 (pno : Word16) (value : Word16) = 
        let ifNoCallback = portWrite pno !><value *> portWrite pno !><(value >>> 8)
        portWrite16Callbacks
        |> Map.tryFind pno
        |> Option.fold (fun _ e -> e pno value |> State.returnM) ifNoCallback : State<unit, Motherboard>
    
    (* Segmented Address calculations *)
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
    
    let getSegOverrideForEA (usess : bool) : State<RegisterSeg, Motherboard> = 
        let innerFn mb = 
            let sr = 
                mb.CPU.SegOverride
                |> Option.orElse (if usess then Some SS
                                  else None)
                |> Option.getOrElse DS
            sr, mb
        innerFn : State<RegisterSeg, Motherboard>

    let setSegOverride sr =
        let innerFn mb = 
            mb.CPU.SegOverride <- Some sr
            (), mb
        innerFn : State<unit, Motherboard>

    let resetSegOverride =
        let innerFn mb = 
            mb.CPU.SegOverride <- None
            (), mb
        innerFn : State<unit, Motherboard>
    
    (* Miscellenous helpers *)
    let inline nyi instr = failwithf "%O - Not implemented" (instr.ToString())
    let inline ns<'T> : State<'T option, Motherboard> = None |> State.returnM
