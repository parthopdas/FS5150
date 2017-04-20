namespace Lib.CPU.Execution

module FDE =
    open YaFunTK 
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
    open Lib.CPU.Execution.String
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open Lib.Parser.Core
    open Lib.Parser.TextInput
    open System.IO
    
    let grammer = 
        (Path.getLocalPath(), "8086_table.txt")
        ||> Path.combine
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
        // TODO: P2D: We are shortcircuting the monad here. How to build a stack of monads like State<Parser<_>>
        runOnInput (pinstruction csip grammer) instrBytes

    let executors =
       [| (* "--", 0x0 *) nyi;
          (* "AAA", 0x1 *) nyi;
          (* "AAD", 0x2 *) nyi;
          (* "AAM", 0x3 *) nyi;
          (* "AAS", 0x4 *) nyi;
          (* "ADC", 0x5 *) nyi;
          (* "ADD", 0x6 *) execADD;
          (* "AND", 0x7 *) execAND;
          (* "CALL", 0x8 *) execCALL;
          (* "CBW", 0x9 *) nyi;
          (* "CLC", 0xA *) execCLC;
          (* "CLD", 0xB *) execCLD;
          (* "CLI", 0xC *) execCLI;
          (* "CMC", 0xD *) execCMC;
          (* "CMP", 0xE *) execCMP;
          (* "CMPSB", 0xF *) CMPSX.execCMPSB;
          (* "CMPSW", 0x10 *) CMPSX.execCMPSW;
          (* "CS:", 0x11 *) execCS;
          (* "CWD", 0x12 *) nyi;
          (* "DAA", 0x13 *) nyi;
          (* "DAS", 0x14 *) nyi;
          (* "DEC", 0x15 *) execDEC;
          (* "DIV", 0x16 *) nyi;
          (* "DS:", 0x17 *) execDS;
          (* "ES:", 0x18 *) execES;
          (* "HLT", 0x19 *) execHLT;
          (* "IDIV", 0x1A *) nyi;
          (* "IMUL", 0x1B *) nyi;
          (* "IN", 0x1C *) execIN;
          (* "INC", 0x1D *) execINC;
          (* "INT", 0x1E *) execINT;
          (* "INTO", 0x1F *) execINTO;
          (* "IRET", 0x20 *) execIRET;
          (* "JA", 0x21 *) execJA;
          (* "JB", 0x22 *) execJB;
          (* "JBE", 0x23 *) execJBE;
          (* "JCXZ", 0x24 *) execJCXZ;
          (* "JG", 0x25 *) execJG;
          (* "JGE", 0x26 *) execJGE;
          (* "JL", 0x27 *) execJL;
          (* "JLE", 0x28 *) execJLE;
          (* "JMP", 0x29 *) execJMP;
          (* "JNB", 0x2A *) execJNB;
          (* "JNO", 0x2B *) execJNO;
          (* "JNS", 0x2C *) execJNS;
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
          (* "LODSB", 0x38 *) LODSX.execLODSB;
          (* "LODSW", 0x39 *) LODSX.execLODSW;
          (* "LOOP", 0x3A *) execLOOP;
          (* "LOOPNZ", 0x3B *) execLOOPNZ;
          (* "LOOPZ", 0x3C *) execLOOPZ;
          (* "MOV", 0x3D *) execMOV;
          (* "MOVSB", 0x3E *) MOVSX.execMOVSB;
          (* "MOVSW", 0x3F *) MOVSX.execMOVSW;
          (* "MUL", 0x40 *) nyi;
          (* "NEG", 0x41 *) nyi;
          (* "NOP", 0x42 *) execNOP;
          (* "NOT", 0x43 *) execNOT;
          (* "OR", 0x44 *) execOR;
          (* "OUT", 0x45 *) execOUT;
          (* "POP", 0x46 *) execPOP;
          (* "POPF", 0x47 *) execPOPF;
          (* "PUSH", 0x48 *) execPUSH;
          (* "PUSHF", 0x49 *) execPUSHF;
          (* "RCL", 0x4A *) nyi;
          (* "RCR", 0x4B *) nyi;
          (* "REPNZ", 0x4C *) execREPNZ;
          (* "REPZ", 0x4D *) execREPZ;
          (* "RET", 0x4E *) execRET;
          (* "RETF", 0x4F *) execRETF;
          (* "ROL", 0x50 *) nyi;
          (* "ROR", 0x51 *) nyi;
          (* "SAHF", 0x52 *) nyi;
          (* "SAR", 0x53 *) nyi;
          (* "SBB", 0x54 *) nyi;
          (* "SCASB", 0x55 *) SCASX.execSCASB;
          (* "SCASW", 0x56 *) SCASX.execSCASW;
          (* "SHL", 0x57 *) execSHL;
          (* "SHR", 0x58 *) execSHR;
          (* "SS:", 0x59 *) execSS;
          (* "STC", 0x5A *) execSTC;
          (* "STD", 0x5B *) execSTD;
          (* "STI", 0x5C *) execSTI;
          (* "STOSB", 0x5D *) STOSX.execSTOSB;
          (* "STOSW", 0x5E *) STOSX.execSTOSW;
          (* "SUB", 0x5F *) execSUB;
          (* "TEST", 0x60 *) execTEST;
          (* "WAIT", 0x61 *) nyi;
          (* "XCHG", 0x62 *) nyi;
          (* "XLAT", 0x63 *) nyi;
          (* "XOR", 0x64 *) execXOR; |]

    /// executeInstr :: Instruction -> State<unit,Motherboard>
    let executeInstr i = 
        executors.[i.OpCode] i
        >>= Option.fold (fun _ e -> e |> State.returnM) (Prelude.flip (|++) i.Length <!> getCSIP)
        >>= setCSIP
