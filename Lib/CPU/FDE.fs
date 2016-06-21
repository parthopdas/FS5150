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
    
    let grammer = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, "8086_table.txt")
        |> File.ReadAllText
        |> InstructionSetLoader.loadInstructionSet
        |> InstructionSetLoader.loadGrammer
    
    let createInputAt a0 = 
        a0
        |> read6Bytes
        |> State.bind (fun bs -> (a0, (fromBytes 0 bs)) |> State.returnM)
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
        runOnInput (pinstruction csip grammer) instrBytes
#endif

    let executors = //<'a> : ((Instruction2 -> State<_, _>) [])= 
       [| (* "--", 0x0 *) nyi;
          (* "AAA", 0x1 *) nyi;
          (* "AAD", 0x2 *) nyi;
          (* "AAM", 0x3 *) nyi;
          (* "AAS", 0x4 *) nyi;
          (* "ADC", 0x5 *) nyi;
          (* "ADD", 0x6 *) execADD;
          (* "AND", 0x7 *) nyi;
          (* "CALL", 0x8 *) execCALL;
          (* "CBW", 0x9 *) nyi;
          (* "CLC", 0xA *) nyi;
          (* "CLD", 0xB *) execCLD;
          (* "CLI", 0xC *) execCLI;
          (* "CMC", 0xD *) nyi;
          (* "CMP", 0xE *) execCMP;
          (* "CMPSB", 0xF *) nyi;
          (* "CMPSW", 0x10 *) nyi;
          (* "CS:", 0x11 *) execXS CS;
          (* "CWD", 0x12 *) nyi;
          (* "DAA", 0x13 *) nyi;
          (* "DAS", 0x14 *) nyi;
          (* "DEC", 0x15 *) nyi;
          (* "DIV", 0x16 *) nyi;
          (* "DS:", 0x17 *) execXS DS;
          (* "ES:", 0x18 *) execXS ES;
          (* "HLT", 0x19 *) nyi;
          (* "IDIV", 0x1A *) nyi;
          (* "IMUL", 0x1B *) nyi;
          (* "IN", 0x1C *) nyi;
          (* "INC", 0x1D *) execINC;
          (* "INT", 0x1E *) nyi;
          (* "INTO", 0x1F *) nyi;
          (* "IRET", 0x20 *) nyi;
          (* "JA", 0x21 *) nyi;
          (* "JB", 0x22 *) execJB;
          (* "JBE", 0x23 *) nyi;
          (* "JCXZ", 0x24 *) execJCXZ;
          (* "JG", 0x25 *) nyi;
          (* "JGE", 0x26 *) nyi;
          (* "JL", 0x27 *) nyi;
          (* "JLE", 0x28 *) nyi;
          (* "JMP", 0x29 *) execJMP;
          (* "JNB", 0x2A *) execJNB;
          (* "JNO", 0x2B *) execJNO;
          (* "JNS", 0x2C *) nyi;
          (* "JNZ", 0x2D *) execJNZ;
          (* "JO", 0x2E *) execJO;
          (* "JPE", 0x2F *) execJPE;
          (* "JPO", 0x30 *) execJPO;
          (* "JS", 0x31 *) execJS;
          (* "JZ", 0x32 *) execJZ;
          (* "LAHF", 0x33 *) nyi;
          (* "LDS", 0x34 *) nyi;
          (* "LEA", 0x35 *) nyi;
          (* "LES", 0x36 *) nyi;
          (* "LOCK", 0x37 *) nyi;
          (* "LODSB", 0x38 *) nyi;
          (* "LODSW", 0x39 *) nyi;
          (* "LOOP", 0x3A *) nyi;
          (* "LOOPNZ", 0x3B *) nyi;
          (* "LOOPZ", 0x3C *) nyi;
          (* "MOV", 0x3D *) execMOV;
          (* "MOVSB", 0x3E *) nyi;
          (* "MOVSW", 0x3F *) nyi;
          (* "MUL", 0x40 *) nyi;
          (* "NEG", 0x41 *) nyi;
          (* "NOP", 0x42 *) nyi;
          (* "NOT", 0x43 *) execNOT;
          (* "OR", 0x44 *) nyi;
          (* "OUT", 0x45 *) execOUT;
          (* "POP", 0x46 *) nyi;
          (* "POPF", 0x47 *) nyi;
          (* "PUSH", 0x48 *) execPUSH;
          (* "PUSHF", 0x49 *) nyi;
          (* "RCL", 0x4A *) nyi;
          (* "RCR", 0x4B *) nyi;
          (* "REPNZ", 0x4C *) nyi;
          (* "REPZ", 0x4D *) execREPX WhileZero;
          (* "RET", 0x4E *) nyi;
          (* "RETF", 0x4F *) nyi;
          (* "ROL", 0x50 *) nyi;
          (* "ROR", 0x51 *) nyi;
          (* "SAHF", 0x52 *) nyi;
          (* "SAR", 0x53 *) nyi;
          (* "SBB", 0x54 *) nyi;
          (* "SCASB", 0x55 *) execSCASB;
          (* "SCASW", 0x56 *) execSCASW;
          (* "SHL", 0x57 *) execSHL;
          (* "SHR", 0x58 *) nyi;
          (* "SS:", 0x59 *) execXS SS;
          (* "STC", 0x5A *) nyi;
          (* "STD", 0x5B *) nyi;
          (* "STI", 0x5C *) nyi;
          (* "STOSB", 0x5D *) execSTOSB;
          (* "STOSW", 0x5E *) execSTOSW;
          (* "SUB", 0x5F *) execSUB;
          (* "TEST", 0x60 *) nyi;
          (* "WAIT", 0x61 *) nyi;
          (* "XCHG", 0x62 *) nyi;
          (* "XLAT", 0x63 *) nyi;
          (* "XOR", 0x64 *) execXOR; |]

    /// executeInstr :: Instruction -> State<unit,Motherboard>
    let executeInstr i = 
#if PERF
        execCLD i
#else
        executors.[i.OpCode] i
        >>= Option.fold (fun _ e -> e |> State.returnM) (Prelude.flip (|++) i.Length <!> getCSIP)
        >>= setCSIP
#endif
