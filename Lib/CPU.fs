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
    let flatten addr =
        ((uint32) addr.Segment <<< 4) + (uint32) addr.Offset |> uint32

    /// incrAddress :: Word16 -> Address -> Address
    let incrAddress n addr =
        { addr with Address.Offset = ((addr.Offset + n) &&& 0xFFFFus) }

    /// readWord8 :: Address -> State<Word8,Motherboard>
    let readWord8 addr =
        let innerFn mb =
            match flatten addr with
            | i when i > 0xFE000u -> mb.BIOS.[(int32)(i - 0xFE000u)], mb
            | _ -> failwithf "Memory area '%s' not accessible" (addr.ToString())
        innerFn : State<Word8,Motherboard> 

    /// readCSIP : State<Address,Motherboard>
    let readCSIP =
        let innerFn mb =
            { Segment = mb.CPU.CS
              Offset = mb.CPU.IP }, mb
        innerFn : State<Address,Motherboard>

    /// fetchInstr : State<(Address * InputState<_>),Motherboard> 
    let fetchInstr =
        let inputFromStartAddr a0 = 
            [for i in 0 .. 5 do yield incrAddress ((uint16)i) a0]
            |> List.map readWord8
            |> State.sequence
            |> State.bind (Array.ofList >> fromBytes { Offset = 0} >> State.returnM)
            |> State.bind (fun is -> (a0, is) |> State.returnM)

        readCSIP 
        |> State.bind inputFromStartAddr : State<(Address * InputState<_>),Motherboard> 
    
    /// decodeInstr :: Address -> InputState<_> -> Result<Instruction * InputState<_>>
    let decodeInstr csip instrBytes = 
        // TODO: P2D: We are shortcircuting the monad here. How to build a stack of monads like State<Parser<_>>
        runOnInput (pinstruction csip instructionSet) instrBytes
    
    /// executeInstr :: Instruction -> State<unit,Motherboard>
    let executeInstr instr = 
        let innerFn mb = 
            mb.CPU.IP <- mb.CPU.IP + (instr |> InstructionSetLoader.instrLen)
            (), mb
        innerFn
    
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
            |> Result.``return``
        
        Result.lift2 (sprintf "%s\n%s") rmbstr rinstr
    
    /// stepCPU :: Motherboard -> Result<Motherboard> 
    let stepCPU mb = 
        mb
        |> State.eval fetchInstr 
        ||> decodeInstr
        |> Result.map fst
        |> Result.bind (executeInstr >> Result.``return``)
        // TODO: P2D: Is short circuting this early OK?
        |> Result.bind (fun s -> State.exec s mb |> Result.``return``)
