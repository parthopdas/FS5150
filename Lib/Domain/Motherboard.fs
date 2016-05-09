namespace Lib.Domain

module PC = 
    open InstructionSet
    open Lib.Parser.Core
    open Lib.Parser.Core.Result
    open Lib.Parser.TextInput
    
    type CPU = 
        { AX : Word16
          BX : Word16
          CX : Word16
          DX : Word16
          SP : Word16
          BP : Word16
          SI : Word16
          DI : Word16
          IP : Word16
          Flags : Word16
          CS : Word16
          DS : Word16
          SS : Word16
          ES : Word16 }
    
    type Memory = Word8 array
    
    type Motherboard = 
        { CPU : CPU
          Memory : Memory }
    
    let initMotherBoard() : Motherboard = 
        { CPU = 
              { AX = 0us
                BX = 0us
                CX = 0us
                DX = 0us
                SP = 0us
                BP = 0us
                SI = 0us
                DI = 0us
                IP = 0us
                Flags = 0us
                CS = 0xFFFFus
                DS = 0us
                SS = 0us
                ES = 0us }
          Memory = Array.zeroCreate (1024 * 1024) }
    
    let fetchInstr (mb : Motherboard) : Address * InputState<_> = failwith "NYI"
    let decodeInstr (csip : Address) (instrBytes : InputState<_>) : Result<Instruction * InputState<_>> = failwith "NYI"
    let executeInstr (instr : Instruction) (mb : Motherboard) : Motherboard = failwith "NYI"
    
    let dumpMotherboard (mb : Motherboard) : Result<string> = 
        let toStr x = x.ToString()
        
        let rinstr = 
            mb
            |> fetchInstr
            ||> decodeInstr
            |>> fst
        returnR (sprintf "%s\n%s") <*> (mb
                                        |> toStr
                                        |> returnR)
        <*> (toStr <!> rinstr)
    
    let stepCPU (mb : Motherboard) : Result<Motherboard> = 
        let rinstr = 
            mb
            |> fetchInstr
            ||> decodeInstr
            |>> fst
        
        let rmb = returnR mb
        returnR executeInstr <*> rinstr <*> rmb
