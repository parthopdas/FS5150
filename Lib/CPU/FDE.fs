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
    
    let instructionSet = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, "8086_table.txt")
        |> File.ReadAllText
        |> InstructionSetLoader.loadInstructionSet
    
    /// fetchInstr : State<(Address * InputState<_>),Motherboard> 
    let fetchInstr = 
        let inputFromStartAddr a0 = 
            [ for i in 0us..5us do
                  yield i |++ a0 ]
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
    let executeInstr instr = 
        match (instr.Mneumonic, executors) ||> Map.tryFind with
        | Some exec -> 
            instr
            |> exec
            >>= Option.fold (fun _ e -> e |> State.returnM) ((|++) instr.Length <!> getCSIP)
            >>= setCSIP
            >>= incrExecedCount
        | None -> nyi instr
