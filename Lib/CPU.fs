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
    open System.Globalization
    open FSharpx.Functional
    
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

    /// executeInstr :: Instruction -> State<unit,Motherboard>
    let executeInstr instr = 
        let failwithnyi instr = failwithf "%O - Not implemented" (instr.ToString())
        let ns = None |> State.returnM
        let exec = 
            match instr.Mneumonic with
            | Mneumonic "JMP" -> 
                match instr.Args with
                | [ ArgAddress a ] -> a |> Some |> State.returnM
                | [ ArgOffset(W16 o) ] -> getCSIP >>= (fun csip -> { csip with Offset = csip.Offset + instr.Length + o } |> Some |> State.returnM)
                | _ -> failwithnyi instr
            | Mneumonic "MOV" -> 
                match instr.Args with
                | [ ArgRegister8 r; ArgImmediate(W8 c) ] -> (setReg8 r c) *> ns
                | [ ArgRegister16 r; ArgImmediate(W16 c) ] -> (setReg16 r c) *> ns
                | [ ArgRegisterSeg r1; ArgRegister16 r2 ] -> ((getReg16 r2) >>= (setRegSeg r1)) *> ns
                | [ ArgDereference dref; ArgImmediate(W16 c) ] -> 
                    (createAddr <!> (getSegOverrideForEA instr.UseSS >>= getRegSeg) <*> getEA dref >>= writeWord16 c) *> ns
                    // TODO: DP2: Implement signed offset 
                | _ -> failwithnyi instr
            | Mneumonic "CLI" -> 
                match instr.Args with
                | [ ] -> setFlag IF false *> ns
                | _ -> failwithnyi instr
            | Mneumonic "CLD" -> 
                match instr.Args with
                | [ ] -> setFlag DF false *> ns
                | _ -> failwithnyi instr
            | Mneumonic "OUT" -> 
                match instr.Args with
                | [ ArgImmediate(W8 c); ArgRegister8 r ] -> (getReg8 r >>= outPort8to8 c) *> ns
                | [ ArgRegister16 pno; ArgRegister8 v ] -> 
                    (getReg16 pno >>= (fun pno -> getReg8 v >>= (outPort8to16 pno))) *> ns
                | _ -> failwithnyi instr
            | Mneumonic "INC" -> 
                match instr.Args with
                | [ ArgRegister8 r ] -> ((+) 1uy <!> getReg8 r >>= setReg8 r) *> ns
                | _ -> failwithnyi instr
            | Mneumonic "XOR" -> 
                match instr.Args with
                | [ ArgRegister16 r1; ArgRegister16 r2 ] -> 
                    (getReg16 r1 >>= (fun r1 -> getReg16 r2 >>= (fun r2 -> r1 ^^^ r2 |> State.returnM))) *> ns
                | _ -> failwithnyi instr
            | _ -> failwithnyi instr

        exec 
        >>= Option.fold (fun _ e -> e |> State.returnM) (incrAddress instr.Length <!> getCSIP) 
        >>= setCSIP
        >>= incrExecedCount
    
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

    type I8088Command = 
        | Trace of AsyncReplyChannel<Result<string>>
        | Register of AsyncReplyChannel<Result<string>>
        | Dump of Address * AsyncReplyChannel<Result<string>>

    let (|TraceCmdFormat|_|) = Regex.tryMatch "t"
    let (|RegisterCmdFormat|_|) = Regex.tryMatch "r"

    let (|DumpCmdFormat|_|) input = 
        match Regex.tryMatch "d ([0-9a-f]{1,4}):([0-9a-f]{1,4})" input with
        | Some am -> 
            Some { Segment = UInt16.Parse(am.Groups.[0].Value, NumberStyles.HexNumber)
                   Offset = UInt16.Parse(am.Groups.[1].Value, NumberStyles.HexNumber) }
        | None -> None

    let inline tee fn x = x |> fn |> ignore; x
    
    type I8088Agent() = 
    
        let processor (inbox : MailboxProcessor<I8088Command>) = 
            let rec nextCmd (mb, br) = 
                let noChangeInBreak = (Prelude.flip Prelude.tuple2 br) |> Result.map
                async { 
                    let! command = inbox.TryReceive (if br then -1 else 0)
                    let f =
                        match command with
                        | Some(Trace(rc)) -> 
                            stepCPU
                            >> tee (Result.bind dumpRegisters >> rc.Reply)
                            >> noChangeInBreak

                        | Some(Register(rc)) -> 
                            tee (dumpRegisters >> rc.Reply) >> Result.unit
                            >> noChangeInBreak

                        | Some(Dump(a, rc)) -> 
                            tee (dumpMemory a >> rc.Reply) >> Result.unit
                            >> noChangeInBreak

                        | None -> 
                            stepCPU 
                            >> Result.bind (fun mb -> (mb, mb.ExecutedCount = 6) |> Result.unit) 

                    return! mb |> f |> loop
                }

            and loop = Result.fold nextCmd (fun _ -> async { return () })

            ()
            |> initMotherBoard
            |> Prelude.flip Prelude.tuple2 false
            |> Result.unit
            |> loop
    
        let mailboxProc = MailboxProcessor.Start processor
        member __.Trace() = mailboxProc.PostAndReply Trace
        member __.Register() = mailboxProc.PostAndReply Register
        member __.Dump a = mailboxProc.PostAndReply(fun rc -> Dump(a, rc))

