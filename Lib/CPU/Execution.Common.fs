namespace Lib.CPU.Execution

module Common = 
    open YaFunTK
    open FSharpx
    open FSharpx.State
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
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
        let innerFn mb = mb.CPU.Flags.[int(flag)], mb
        innerFn : State<bool, Motherboard>
    
    let setFlag (flag : Flags) value = 
        let innerFn mb = 
            mb.CPU.Flags.[int(flag)] <- value
            (), mb
        innerFn : State<unit, Motherboard>
    
    let setCSIP addr = 
        let innerFn mb = 
            mb.CPU.CS <- addr.Segment
            mb.CPU.IP <- addr.Offset
            (), mb
        innerFn : State<unit, Motherboard>
    
    // TODO: Perf: Split this out into getcs and getip, same for set
    let getCSIP = 
        let innerFn mb = 
            mb.CPU.CS @|@ mb.CPU.IP, mb
        innerFn : State<Address, Motherboard>
    
    let setSSSP addr = 
        let innerFn mb = 
            mb.CPU.SS <- addr.Segment
            mb.CPU.SP <- addr.Offset
            (), mb
        innerFn : State<unit, Motherboard>
    
    let getSSSP = 
        let innerFn mb = 
            mb.CPU.SS @|@ mb.CPU.SP, mb
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
    
    let getRegFlags = 
        let innerFn mb =
            let data = uint16 mb.CPU.Flags.Data 
            data, mb
        innerFn : State<Word16, Motherboard>
    
    let setRegFlags (value : Word16) = 
        let innerFn mb = 
            mb.CPU.Flags <- value |> int |> BitVector32
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
    
    // TODO: PERF: Change to array indexed by AX, BX, etc.
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
        innerFn : State<Word8, Motherboard>
    
    let read6Bytes addr = 
        let innerFn mb = 
            let a0 = flatten (addr)
            let bs : Word8[] = Array.zeroCreate 6 
            Array.blit mb.RAM ((int)a0) bs 0 6
            bs, mb
        innerFn : State<Word8[], Motherboard>
        
    let readWord16 addr = 
        (+|+) <!> readWord8 (addr |++ 1us) <*> readWord8 addr
    
    let writeWord8 value addr = 
        let innerFn mb = 
            let addr = int32 (flatten addr)
            if not mb.ReadOnly.[addr] then mb.RAM.[addr] <- value
            (), mb
        innerFn : State<unit, Motherboard>
    
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
        innerFn : State<Word8, Motherboard>
    
    let portRead16Callbacks : Map<Word16, Word16 -> Word16> = Map.empty
    
    let portRead16 (pno : Word16) = 
        let ifNoCallback = (+|+) <!> portRead8 (pno + 1us) <*> portRead8 pno
        portRead16Callbacks
        |> Map.tryFind pno
        |> Option.fold (fun _ e -> e pno |> State.returnM) ifNoCallback : State<Word16, Motherboard>
    
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
        innerFn : State<unit, Motherboard>
    
    let portWrite16Callbacks : Map<Word16, Word16 -> Word16 -> unit> = Map.empty
    
    let portWrite16 (pno : Word16) (value : Word16) = 
        let ifNoCallback = portWrite8 pno !><value *> portWrite8 pno !><(value >>> 8)
        portWrite16Callbacks
        |> Map.tryFind pno
        |> Option.fold (fun _ e -> e pno value |> State.returnM) ifNoCallback : State<unit, Motherboard>
    
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
                         | W8 w8 -> Lib.Common.signExtend w8
                         | W16 w16 -> w16) 0us
            |> State.returnM
        
        (+) <!> reg <*> disp
    
    let private getSegOverrideForEA (usess : bool) : State<RegisterSeg, Motherboard> = 
        let innerFn mb = 
            let sr = 
                mb.CPU.SegmentOverride
                |> Option.orElse (if usess then Some SS
                                  else None)
                |> Option.getOrElse DS
            sr, mb
        innerFn : State<RegisterSeg, Motherboard>
    
    // TODO: Unit tests for AddressFromDref - http://www.jagregory.com/abrash-zen-of-asm/#mod-reg-rm-addressing
    let addressFromDref instr t d = 
        (@|@) <!> (getSegOverrideForEA instr.UseSS >>= getRegSeg) <*> getEA t d
    
    let getSegOverride def = 
        let innerFn mb = 
            let seg = 
                mb.CPU.SegmentOverride
                |> Option.getOrElse def
            seg, mb
        innerFn : State<RegisterSeg, Motherboard>
    
    let setSegOverride sr = 
        let innerFn mb = 
            mb.CPU.SegmentOverride <- Some sr
            (), mb
        innerFn : State<unit, Motherboard>

    let inline readMem8 instr dref = addressFromDref instr dref.DrefType8 dref.DrefDisp8 >>= readWord8
    let inline readMem16 instr dref = addressFromDref instr dref.DrefType16 dref.DrefDisp16 >>= readWord16
    let inline writeMem8 instr dref = fun v -> (addressFromDref instr dref.DrefType8 dref.DrefDisp8 >>= writeWord8 v)
    let inline writeMem16 instr dref = fun v -> (addressFromDref instr dref.DrefType16 dref.DrefDisp16 >>= writeWord16 v)
    
    (*  CPU State management *)
    let getLogicalInstrStart =
        let innerFn mb = 
            mb.CPU.LogicalInstrStart, mb
        innerFn : State<Address, Motherboard>
     
    let beforeLogicalInstr = 
        let innerFn mb = 
            mb.CPU.LogicalInstrStart <- mb.CPU.CS @|@ mb.CPU.IP
            mb.CPU.SegmentOverride <- None
            mb.CPU.RepetitionType <- NoRepetition
            (), mb
        innerFn : State<unit, Motherboard>
    
    let beforePhysicalInstr = 
        let innerFn mb = 
            mb.SW.Restart()
            (), mb
        innerFn : State<unit, Motherboard>
    
    let afterPhysicalInstr = 
        let innerFn mb = 
            mb.CPU.ICount <- mb.CPU.ICount + 1L
            mb.SW.Stop()
            mb.CPU.ITicks <- mb.CPU.ITicks + mb.SW.ElapsedTicks
            (), mb
        innerFn : State<unit, Motherboard>
    
    let resetCPU = 
        let innerFn mb = 
            let cpu = initialCPU()
            cpu.ICount <- mb.CPU.ICount
            cpu.ITicks <- mb.CPU.ITicks
            (), { mb with CPU = cpu }
        innerFn : State<unit, Motherboard>
    
    let setRepetitionType rt = 
        let innerFn mb = 
            mb.CPU.RepetitionType <- rt
            (), mb
        innerFn : State<unit, Motherboard>
    
    let getRepetitionType = 
        let innerFn mb = 
            mb.CPU.RepetitionType, mb
        innerFn : State<RepetitionType, Motherboard>
    
    let setHalted = 
        let innerFn mb = 
            mb.CPU.Halted <- true
            (), mb
        innerFn : State<unit, Motherboard>
    
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
    let inline ns<'T> : State<'T option, Motherboard> = None |> State.returnM
