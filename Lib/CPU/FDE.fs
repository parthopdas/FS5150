namespace Lib.CPU.Execution

module FDE = 
    open FSharpx
    open FSharpx.State
    open Lib.CPU
    open Lib.CPU.Disassembler
    open Lib.CPU.Execution.Arithmetic
    open Lib.CPU.Execution.Common
    open Lib.CPU.Execution.Control
    open Lib.CPU.Execution.Data
    open Lib.CPU.Execution.Logic
    open Lib.CPU.Execution.Processor
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open Lib.Parser.Core
    open Lib.Parser.TextInput
    open System
    open System.IO
    open System.Reflection
    open FSharpx.Functional
    
    let instructionSet = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, "8086_table.txt")
        |> File.ReadAllText
        |> InstructionSetLoader.loadInstructionSet
    
    let createInputAt a0 = 
        a0
        |> read6Bytes
        |> State.bind (fun bs -> (a0, (fromBytes { Offset = 0 } bs)) |> State.returnM)
        : State<(Address * InputState<_>),Motherboard> 

    /// fetchInstr : State<(Address * InputState<_>),Motherboard> 
    let createInputAtCSIP = 
        getCSIP >>= createInputAt : State<Address * InputState<_>, Motherboard>
    
    /// decodeInstr :: Address -> InputState<_> -> Result<Instruction * InputState<_>>
    let decodeInstr csip instrBytes = 
#if PERF
        ({ Address = { Segment = 0us; Offset = 0us }
           Mneumonic = "CLD"
           IsPrefix = false
           UseSS = false
           Args = []
           Bytes = [| 0x26uy |] }, fromBytes [|0us; 0us; 0us; 0us; 0us; 0us|]) |> Result.returnM
#else
        // TODO: P2D: We are shortcircuting the monad here. How to build a stack of monads like State<Parser<_>>
        runOnInput (pinstruction csip instructionSet) instrBytes
#endif

    let executors = 
        [ (// Data
           "MOV", execMOV)
          ("OUT", execOUT)
          ("CS:", execXS)
          ("DS:", execXS)
          ("ES:", execXS)
          ("SS:", execXS)
          (// Arithmetic
           "ADD", execADD)
          ("SUB", execSUB)
          ("CMP", execCMP)
          ("INC", execINC)
          (// Logic
           "NOT", execNOT)
          ("SHL", execSHL)
          ("XOR", execXOR)
          (// String
           // Control
           "JMP", execJMP)
          ("JB", execJB)
          ("JNB", execJNB)
          ("JO", execJO)
          ("JNO", execJNO)
          ("JS", execJS)
          ("JZ", execJZ)
          ("JNZ", execJNZ)
          ("JPE", execJPE)
          ("JPO", execJPO)
          (// Processor
           "CLI", execCLI)
          ("CLD", execCLD) ]
        |> Map.ofList
    
    /// executeInstr :: Instruction -> State<unit,Motherboard>
    let inline executeInstr instr = 
#if PERF
        execCLD instr
#else
        match (instr.Mneumonic, executors) ||> Map.tryFind with
        | Some executor -> 
            executor instr
            >>= Option.fold (fun _ e -> e |> State.returnM) ((|++) instr.Length <!> getCSIP)
            >>= setCSIP
        | None -> nyi instr
#endif