namespace Lib.Chips.I8088.Execution

module Common = 
    open YaFunTK
    open FSharpx
    open FSharpx.State
    open Lib.Chips.I8088.InstructionSet
    open Lib.Chips.I8088
    open System.Collections.Specialized
    
    let initialCPU() = 
        { AX = 0us
          BX = 3us
          CX = 1us
          DX = 2us
          SP = 0us
          BP = 0us
          SI = 0us
          DI = 0us
          IP = 0us
          CS = 0us
          DS = 0us
          SS = 0us
          ES = 0us
          Flags = BitVector32()
          LogicalInstrStart = { Offset = 0us; Segment = 0us }
          SegmentOverride = None
          RepetitionType = NoRepetition
          Halted = false
          ITicks = 0L
          ICount = 0L }
    
    (* Byte/Word/Address manipulation *)
    let inline (!<>) (w8 : Word8) : Word16 = (Word16) w8
    let inline (!><) (w16 : Word16) : Word8 = (Word8) w16
    let inline (+|+) (hi : Word8) (lo : Word8) : Word16 = !<>hi <<< 8 ||| !<>lo
    let getHiByte : Word16 -> Word8 = Prelude.flip (>>>) 8 >> (!><)
    let getLoByte : Word16 -> Word8 = Prelude.flip (&&&) 0x00FFus >> (!><)
    let inline setHiByte w16 (value : Word8) : Word16 = value +|+ getLoByte w16
    let inline setLoByte w16 (value : Word8) : Word16 = getHiByte w16 +|+ value
    
    let inline flatten addr = 
        ((uint32) addr.Segment <<< 4) + (uint32) addr.Offset
        |> Prelude.flip (&&&) 0xFFFFFu
        |> uint32
    
    let inline (|++) addr n = { addr with Address.Offset = ((addr.Offset + n) &&& 0xFFFFus) }
    let inline (|--) addr n = { addr with Address.Offset = ((addr.Offset - n) &&& 0xFFFFus) }
    
    let inline (@|@) s o =
        { Segment = s
          Offset = o }
    
    (* Register IO *)
    let getFlag (flag : Flags) = 
        let innerFn mb = mb.Registers.Flags.[int(flag)], mb
        innerFn : State<bool, I8088>
    
    let setFlag (flag : Flags) value = 
        let innerFn mb = 
            mb.Registers.Flags.[int(flag)] <- value
            (), mb
        innerFn : State<unit, I8088>
    
    let setCSIP addr = 
        let innerFn mb = 
            mb.Registers.CS <- addr.Segment
            mb.Registers.IP <- addr.Offset
            (), mb
        innerFn : State<unit, I8088>
    
    // TODO: Perf: Split this out into getcs and getip, same for set
    let getCSIP = 
        let innerFn mb = 
            mb.Registers.CS @|@ mb.Registers.IP, mb
        innerFn : State<Address, I8088>
    
    let setSSSP addr = 
        let innerFn mb = 
            mb.Registers.SS <- addr.Segment
            mb.Registers.SP <- addr.Offset
            (), mb
        innerFn : State<unit, I8088>
    
    let getSSSP = 
        let innerFn mb = 
            mb.Registers.SS @|@ mb.Registers.SP, mb
        innerFn : State<Address, I8088>
    
    let getRegSeg segReg = 
        let innerFn mb = 
            let data = 
                match segReg with
                | CS -> mb.Registers.CS
                | DS -> mb.Registers.DS
                | ES -> mb.Registers.ES
                | SS -> mb.Registers.SS
            data, mb
        innerFn : State<Word16, I8088>
    
    let setRegSeg segReg value = 
        let innerFn mb = 
            match segReg with
            | CS -> mb.Registers.CS <- value
            | DS -> mb.Registers.DS <- value
            | ES -> mb.Registers.ES <- value
            | SS -> mb.Registers.SS <- value
            (), mb
        innerFn : State<unit, I8088>
    
    let getRegFlags = 
        let innerFn mb =
            let data = uint16 mb.Registers.Flags.Data 
            data, mb
        innerFn : State<Word16, I8088>
    
    let setRegFlags (value : Word16) = 
        let innerFn mb = 
            mb.Registers.Flags <- value |> int |> BitVector32
            (), mb
        innerFn : State<unit, I8088>
    
    let getReg8 reg = 
        let innerFn mb = 
            let data = 
                match reg with
                | AL -> getLoByte mb.Registers.AX
                | AH -> getHiByte mb.Registers.AX
                | BL -> getLoByte mb.Registers.BX
                | BH -> getHiByte mb.Registers.BX
                | CL -> getLoByte mb.Registers.CX
                | CH -> getHiByte mb.Registers.CX
                | DL -> getLoByte mb.Registers.DX
                | DH -> getHiByte mb.Registers.DX
            data, mb
        innerFn : State<Word8, I8088>
    
    let setReg8 reg value = 
        let innerFn mb = 
            match reg with
            | AL -> mb.Registers.AX <- setLoByte mb.Registers.AX value
            | AH -> mb.Registers.AX <- setHiByte mb.Registers.AX value
            | BL -> mb.Registers.BX <- setLoByte mb.Registers.BX value
            | BH -> mb.Registers.BX <- setHiByte mb.Registers.BX value
            | CL -> mb.Registers.CX <- setLoByte mb.Registers.CX value
            | CH -> mb.Registers.CX <- setHiByte mb.Registers.CX value
            | DL -> mb.Registers.DX <- setLoByte mb.Registers.DX value
            | DH -> mb.Registers.DX <- setHiByte mb.Registers.DX value
            (), mb
        innerFn : State<unit, I8088>
    
    // TODO: PERF: Change to array indexed by AX, BX, etc.
    let getReg16 reg = 
        let innerFn mb = 
            let data = 
                match reg with
                | AX -> mb.Registers.AX
                | BX -> mb.Registers.BX
                | CX -> mb.Registers.CX
                | DX -> mb.Registers.DX
                | SP -> mb.Registers.SP
                | BP -> mb.Registers.BP
                | SI -> mb.Registers.SI
                | DI -> mb.Registers.DI
            data, mb
        innerFn : State<Word16, I8088>
    
    let setReg16 reg value = 
        let innerFn mb = 
            match reg with
            | AX -> mb.Registers.AX <- value
            | BX -> mb.Registers.BX <- value
            | CX -> mb.Registers.CX <- value
            | DX -> mb.Registers.DX <- value
            | SP -> mb.Registers.SP <- value
            | BP -> mb.Registers.BP <- value
            | SI -> mb.Registers.SI <- value
            | DI -> mb.Registers.DI <- value
            (), mb
        innerFn : State<unit, I8088>
    
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
    
    let inline flagSZP<'T 
            when 'T : equality 
             and 'T : (static member ( &&& ) :  ^T *  ^T ->  ^T)> 
            (v: 'T) (zero: 'T) (mid: 'T) (loByte : ^T -> uint8) = 
        (setFlag Flags.SF (v &&& mid = mid)) 
        *> (setFlag Flags.ZF (v = zero)) 
        *> (setFlag Flags.PF (parity.[v |> loByte |> int]))
    
    let inline flagSZP8 (w8 : Word8) = 
        flagSZP w8 0uy 0x80uy id

    let inline flagSZP16 (w16 : Word16) = 
        flagSZP w16 0us 0x8000us getLoByte
    
    (* Memory IO *)
    let readWord8 addr = 
        let innerFn mb = 
            let addr = int32 (flatten addr)
            mb.RAM.[addr], mb
        innerFn : State<Word8, I8088>
    
    let read6Bytes addr = 
        let innerFn mb = 
            let a0 = flatten (addr)
            let bs : Word8[] = Array.zeroCreate 6 
            Array.blit mb.RAM ((int)a0) bs 0 6
            bs, mb
        innerFn : State<Word8[], I8088>
        
    let readWord16 addr = 
        (+|+) <!> readWord8 (addr |++ 1us) <*> readWord8 addr
    
    let writeWord8 value addr = 
        let innerFn mb = 
            let addr = int32 (flatten addr)
            if not mb.ReadOnly.[addr] then mb.RAM.[addr] <- value
            (), mb
        innerFn : State<unit, I8088>
    
    let writeWord16 (value : Word16) addr = 
        (writeWord8 (getLoByte value) addr) >>. (writeWord8 (getHiByte value) (addr |++ 1us))
    
    let inline push w16 = 
        getSSSP >>= (fun sssp -> 
                     let sssp' = sssp |-- 2us
                     setSSSP sssp' *> writeWord16 w16 sssp')
    
    let pop = 
        getSSSP >>= (fun sssp -> 
                     let sssp' = sssp |++ 2us
                     readWord16 sssp <* setSSSP sssp')

    (* Device IO *)
    let portReadCallbacks : Map<Word16, Word16 -> Word8> = Map.empty
    
    let portRead8 (pno : Word16) = 
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
        innerFn : State<Word8, I8088>
    
    let portRead16Callbacks : Map<Word16, Word16 -> Word16> = Map.empty
    
    let portRead16 (pno : Word16) = 
        let ifNoCallback = (+|+) <!> portRead8 (pno + 1us) <*> portRead8 pno
        portRead16Callbacks
        |> Map.tryFind pno
        |> Option.fold (fun _ e -> e pno |> State.returnM) ifNoCallback : State<Word16, I8088>
    
    let portWriteCallbacks : Map<Word16, Word16 -> Word8 -> unit> = Map.empty
    
    let portWrite8 (pno : Word16) (value : Word8) = 
        let innerFn mb = 
            mb.PortRAM.[(int) pno] <- value
            match pno with
            | 0x61us -> () // TODO: P2D: dprintfn "Writing to port 61 (%d)- Enabling/disabing speaker NYI" value
            | _ -> ()
            portWriteCallbacks
            |> Map.tryFind pno
            |> Option.fold (fun _ e -> e pno value) ()
            (), mb
        innerFn : State<unit, I8088>
    
    let portWrite16Callbacks : Map<Word16, Word16 -> Word16 -> unit> = Map.empty
    
    let portWrite16 (pno : Word16) (value : Word16) = 
        let ifNoCallback = portWrite8 pno !><value *> portWrite8 pno !><(value >>> 8)
        portWrite16Callbacks
        |> Map.tryFind pno
        |> Option.fold (fun _ e -> e pno value |> State.returnM) ifNoCallback : State<unit, I8088>
    
    (* Segmented Address calculations *)
    let private getEA t d = 
        let reg = 
            match t with
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
            d
            |> Option.fold (fun acc e -> 
                   acc + match e with
                         | W8 w8 -> Common.signExtend w8
                         | W16 w16 -> w16) 0us
            |> State.returnM
        
        (+) <!> reg <*> disp
    
    let private getSegOverrideForEA (usess : bool) : State<RegisterSeg, I8088> = 
        let innerFn mb = 
            let sr = 
                mb.Registers.SegmentOverride
                |> Option.orElse (if usess then Some SS
                                  else None)
                |> Option.getOrElse DS
            sr, mb
        innerFn : State<RegisterSeg, I8088>
    
    // TODO: Unit tests for AddressFromDref - http://www.jagregory.com/abrash-zen-of-asm/#mod-reg-rm-addressing
    let addressFromDref instr t d = 
        (@|@) <!> (getSegOverrideForEA instr.UseSS >>= getRegSeg) <*> getEA t d
    
    let getSegOverride def = 
        let innerFn mb = 
            let seg = 
                mb.Registers.SegmentOverride
                |> Option.getOrElse def
            seg, mb
        innerFn : State<RegisterSeg, I8088>
    
    let setSegOverride sr = 
        let innerFn mb = 
            mb.Registers.SegmentOverride <- Some sr
            (), mb
        innerFn : State<unit, I8088>

    let inline readMem8 instr dref = addressFromDref instr dref.DrefType8 dref.DrefDisp8 >>= readWord8
    let inline readMem16 instr dref = addressFromDref instr dref.DrefType16 dref.DrefDisp16 >>= readWord16
    let inline writeMem8 instr dref = fun v -> (addressFromDref instr dref.DrefType8 dref.DrefDisp8 >>= writeWord8 v)
    let inline writeMem16 instr dref = fun v -> (addressFromDref instr dref.DrefType16 dref.DrefDisp16 >>= writeWord16 v)
    
    (*  CPU State management *)
    let getLogicalInstrStart =
        let innerFn mb = 
            mb.Registers.LogicalInstrStart, mb
        innerFn : State<Address, I8088>
     
    let beforeLogicalInstr = 
        let innerFn mb = 
            mb.Registers.LogicalInstrStart <- mb.Registers.CS @|@ mb.Registers.IP
            mb.Registers.SegmentOverride <- None
            mb.Registers.RepetitionType <- NoRepetition
            (), mb
        innerFn : State<unit, I8088>
    
    let beforePhysicalInstr = 
        let innerFn mb = 
            mb.SW.Restart()
            (), mb
        innerFn : State<unit, I8088>
    
    let afterPhysicalInstr = 
        let innerFn mb = 
            mb.Registers.ICount <- mb.Registers.ICount + 1L
            mb.SW.Stop()
            mb.Registers.ITicks <- mb.Registers.ITicks + mb.SW.ElapsedTicks
            (), mb
        innerFn : State<unit, I8088>
    
    let resetCPU = 
        let innerFn mb = 
            let cpu = initialCPU()
            cpu.ICount <- mb.Registers.ICount
            cpu.ITicks <- mb.Registers.ITicks
            (), { mb with Registers = cpu }
        innerFn : State<unit, I8088>
    
    let setRepetitionType rt = 
        let innerFn mb = 
            mb.Registers.RepetitionType <- rt
            (), mb
        innerFn : State<unit, I8088>
    
    let getRepetitionType = 
        let innerFn mb = 
            mb.Registers.RepetitionType, mb
        innerFn : State<RepetitionType, I8088>
    
    let setHalted = 
        let innerFn mb = 
            mb.Registers.Halted <- true
            (), mb
        innerFn : State<unit, I8088>
    
    (* Common arithmetic *)
    let private sub8ValParams = (0xFF00us, 0us, 0x80us, 0x10us)
    let private sub8FunParams = ((-), uint8, uint16)
    let private sub16ValParams = (0xFFFF0000ul, 0ul, 0x8000ul, 0x10ul)
    let private sub16FunParams = ((-), uint16, uint32)

    let inline private setSubFlags<'T, 'TUp
                        when 'TUp : equality
                         and 'TUp : (static member ( ^^^ ) :  ^TUp *  ^TUp ->  ^TUp)
                         and 'TUp : (static member ( &&& ) :  ^TUp *  ^TUp ->  ^TUp)>
            (op : 'TUp -> 'TUp -> 'TUp, fdn : 'TUp -> 'T, fup : 'T -> 'TUp) setSZPFlags (vff00, v0, vMid, v10) (v1 : 'T) (v2 : 'T) = 
        let dst = op (fup(v1)) (fup(v2))
        (setSZPFlags (fdn(dst))) 
        *> (setFlag Flags.CF (dst &&& vff00 <> v0)) 
        *> (setFlag Flags.OF (((dst ^^^ fup(v1)) &&& (fup(v1) ^^^ fup(v2)) &&& vMid) <> v0)) 
        *> (setFlag Flags.AF (((fup(v1) ^^^ fup(v2) ^^^ dst) &&& v10) <> v0))

    let setSub8Flags = 
        setSubFlags sub8FunParams flagSZP8 sub8ValParams

    let setSub16Flags = 
        setSubFlags sub16FunParams flagSZP16 sub16ValParams
    
    (* Miscellenous helpers *)
    // TODO: How do we get the type system to take care of OpCode signatures so we dont have this catch all for all opcodes?
    let inline nyi instr = failwithf "%O - Not implemented" (instr.ToString())
    let inline ns<'T> : State<'T option, I8088> = None |> State.returnM
