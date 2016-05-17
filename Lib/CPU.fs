namespace Lib

module CPU = 
    open Disassembler
    open FSharpx
    open FSharpx.State
    open FSharpx.Text
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open Lib.Parser.Core
    open Lib.Parser.TextInput
    open System
    open System.IO
    open System.Reflection
    
    let instructionSet = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, "8086_table.txt")
        |> File.ReadAllText
        |> InstructionSetLoader.loadInstructionSet
    
    /// flatten :: Address -> uint32
    let private flatten addr = 
        ((uint32) addr.Segment <<< 4) + (uint32) addr.Offset
        |> (&&&) 0xFFFFFu
        |> uint32
    
    let inline getHiByte w16 =
        w16 >>> 8 |> uint8 : Word8
    
    let inline getLoByte w16 =
        w16 &&& 0x00FFus |> uint8 : Word8
    
    let inline setHiByte w16 (value : Word8) =
        ((uint16)value <<< 8) + (uint16)(getLoByte w16) : Word16
    
    let inline setLoByte w16 (value : Word8) =
        (uint16)(getHiByte w16) + ((uint16)value) : Word16
    
    /// createAddr Word16 -> Word16 -> Address
    let createAddr segment offset = 
        { Segment = segment
          Offset = offset }
    
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
    
    /// getCSIP : State<Address,Motherboard>
    let getCSIP = 
        let innerFn mb = 
            { Segment = mb.CPU.CS
              Offset = mb.CPU.IP }, mb
        innerFn : State<Address, Motherboard>
    
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
    
    /// incrIP : Word16 -> State<unit,Motherboard>
    let incrIP n = 
        let innerFn mb = 
            mb.CPU.IP <- (mb.CPU.IP + n) &&& 0xFFFFus
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
    
    /// fetchInstr : State<(Address * InputState<_>),Motherboard> 
    let fetchInstr = 
        let inputFromStartAddr a0 = 
            [ for i in 0..5 do
                  yield incrAddress ((uint16) i) a0 ]
            |> List.map readWord8
            |> State.sequence
            |> State.bind (Array.ofList
                           >> fromBytes { Offset = 0 }
                           >> State.returnM)
            |> State.bind (fun is -> (a0, is) |> State.returnM)
        getCSIP |> State.bind inputFromStartAddr : State<Address * InputState<_>, Motherboard>
    
    /// decodeInstr :: Address -> InputState<_> -> Result<Instruction * InputState<_>>
    let decodeInstr csip instrBytes = 
        // TODO: P2D: We are shortcircuting the monad here. How to build a stack of monads like State<Parser<_>>
        runOnInput (pinstruction csip instructionSet) instrBytes
    
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

    /// executeInstr :: Instruction -> State<unit,Motherboard>
    let executeInstr instr = 
        let failwithnyi instr = failwithf "%O - Not implemented" (instr.ToString())
        match instr.Mneumonic with
        | Mneumonic "JMP" -> 
            match instr.Args with
            | [ ArgAddress a ] -> setCSIP a
            | _ -> failwithnyi instr
        | Mneumonic "MOV" -> 
            match instr.Args with
            | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> (setReg8 r c) *> (incrIP instr.Length)
            | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> (setReg16 r c) *> (incrIP instr.Length)
            | [ ArgRegisterSeg r1; ArgRegister16 r2 ] -> ((getReg16 r2) >>= (setRegSeg r1)) *> (incrIP instr.Length)
            | [ ArgDereference dref; ArgImmediate(W16 c) ] -> 
                (createAddr <!> (getSegOverrideForEA instr.UseSS >>= getRegSeg) <*> getEA dref >>= writeWord16 c) *> (incrIP instr.Length)
                // TODO: DP2: Implement signed offset 
            | _ -> failwithnyi instr
        | Mneumonic "CLI" -> 
            match instr.Args with
            | [ ] -> setFlag IF false *> (incrIP instr.Length)
            | _ -> failwithnyi instr
        | Mneumonic "CLD" -> 
            match instr.Args with
            | [ ] -> setFlag DF false *> (incrIP instr.Length)
            | _ -> failwithnyi instr
        | _ -> failwithnyi instr
    
    /// stepCPU :: Motherboard -> Result<Motherboard> 
    let stepCPU mb = 
        mb
        |> State.eval fetchInstr
        ||> decodeInstr
        |> Result.map fst
        |> Result.bind (executeInstr >> Result.unit)
        // TODO: P2D: Is short circuting this early OK?
        |> Result.bind (fun s -> State.exec s mb |> Result.unit)
    
    /// dumpRegisters :: Motherboard -> Result<string>
    let dumpRegisters mb = 
        let toStr x = x.ToString()
        
        let rinstr = 
            mb
            |> State.eval fetchInstr
            ||> decodeInstr
            |> Result.map fst
            |> Result.map toStr
        
        let rmbstr = 
            mb
            |> toStr
            |> Result.unit
        
        Result.lift2 (sprintf "%s\n%s") rmbstr rinstr
    
    /// dumpMemory :: Address -> Motherboard -> Result<string>
    let dumpMemory addr mb = 
        let bytesToPrint = 128
        let bytesPerLine = 0x10
        
        let sbs = 
            [ for i in 0..(bytesToPrint - 1) do
                  yield incrAddress ((uint16) i) addr ]
            |> List.map readWord8
            |> State.sequence
        
        let l1 = 
            mb
            |> State.eval sbs
            |> List.mapi (fun i e -> i / bytesPerLine, e)
        
        let m1 = 
            List.foldBack (fun (eK, eV) acc -> 
                let m, l = 
                    acc
                    |> Map.tryFind eK
                    |> Option.fold (fun _ e -> (Map.remove eK acc), e) (acc, [])
                m |> Map.add eK (eV :: l)) l1 Map.empty
        
        let lines = 
            Map.foldBack (fun eK eT acc -> 
                let a = incrAddress ((uint16) (eK * bytesPerLine)) addr
                
                let bstr = 
                    eT
                    |> List.map (sprintf "%02x ")
                    |> String.concat ""
                
                let pstr = 
                    eT
                    |> List.map (fun w -> 
                           let c = (char) w
                           if Char.IsControl(c) then "."
                           else c.ToString())
                    |> String.concat ""
                
                (sprintf "%O %O %O" a bstr pstr) :: acc) m1 []
        
        lines
        |> Strings.joinLines
        |> Result.unit
