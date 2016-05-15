namespace Lib

module CPU = 
    open Disassembler
    open FSharpx
    open FSharpx.State
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
    let flatten addr = ((uint32) addr.Segment <<< 4) + (uint32) addr.Offset |> uint32
    
    /// incrAddress :: Word16 -> Address -> Address
    let incrAddress n addr = { addr with Address.Offset = ((addr.Offset + n) &&& 0xFFFFus) }
    
    /// readWord8 :: Address -> State<Word8,Motherboard>
    let readWord8 addr = 
        let innerFn mb = 
            match flatten addr with
            | i when i > 0xFE000u -> mb.BIOS.[(int32) (i - 0xFE000u)], mb
            | _ -> failwithf "Memory area '%s' not accessible" (addr.ToString())
        innerFn : State<Word8, Motherboard>
    
    /// getCSIP : State<Address,Motherboard>
    let getCSIP = 
        let innerFn mb = 
            { Segment = mb.CPU.CS
              Offset = mb.CPU.IP }, mb
        innerFn : State<Address, Motherboard>
    
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
    
    /// setReg16 : Regiter -> Word16 -> State<unit,Motherboard>
    let setReg16 reg data = 
        let innerFn mb = 
            match reg with
            | AX -> mb.CPU.AX <- data
            | BX -> mb.CPU.BX <- data
            | CX -> mb.CPU.CX <- data
            | DX -> mb.CPU.DX <- data
            | SP -> mb.CPU.SP <- data
            | BP -> mb.CPU.BP <- data
            | SI -> mb.CPU.SI <- data
            | DI -> mb.CPU.DI <- data
            | CS -> mb.CPU.CS <- data
            | DS -> mb.CPU.DS <- data
            | ES -> mb.CPU.ES <- data
            | SS -> mb.CPU.SS <- data
            | IP -> mb.CPU.IP <- data
            | _ -> failwithf "%A is not a 16 bit register" reg
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
                | CS -> mb.CPU.CS
                | DS -> mb.CPU.DS
                | ES -> mb.CPU.ES
                | SS -> mb.CPU.SS
                | IP -> mb.CPU.IP
                | _ -> failwithf "%A is not a 16 bit register" reg
            data, mb
        innerFn : State<Word16, Motherboard>
    
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
    
    let failwithnyi instr = failwithf "%O - Not implemented" (instr.ToString())

    /// executeInstr :: Instruction -> State<unit,Motherboard>
    let executeInstr instr = 
        match instr.Mneumonic with
        | Mneumonic "JMP" -> 
            match instr.Args with
            | [ ArgAddress a ] -> setCSIP a
            | _ -> failwithnyi instr
        | Mneumonic "MOV" -> 
            match instr.Args with
            | [ ArgRegister AX; ArgImmediate(W16 c) ] -> (setReg16 AX c) *> (incrIP instr.Length)
            | [ ArgRegister r1; ArgRegister r2 ] -> ((getReg16 r2) >>= (setReg16 r1)) *> (incrIP instr.Length)
            | _ -> failwithnyi instr
        | _ -> failwithnyi instr
    
    /// dumpMotherboard :: Motherboard -> Result<string>
    let dumpMotherboard mb = 
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
    
    /// stepCPU :: Motherboard -> Result<Motherboard> 
    let stepCPU mb = 
        mb
        |> State.eval fetchInstr
        ||> decodeInstr
        |> Result.map fst
        |> Result.bind (executeInstr >> Result.unit)
        // TODO: P2D: Is short circuting this early OK?
        |> Result.bind (fun s -> State.exec s mb |> Result.unit)
