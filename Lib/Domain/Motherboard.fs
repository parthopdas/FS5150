namespace Lib.Domain

module PC = 
    open InstructionSet
    open System
    open System.Collections.Generic
    open System.IO
    open System.Reflection
    open System.Diagnostics
    
    type Flags = 
        /// Overflow - Signed number exceeds capacity of result
        | OF
        /// Direction - Set by user to indication direction of string instructions
        | DF
        /// Interrupt - Enable or disable hardware interrupts
        | IF
        /// Trap - Single step
        | TF
        /// Sign - Results sign bit from compare/substract ops
        | SF
        /// Zero - Result is zero from compare/substract ops
        | ZF
        /// Adjust - ?
        | AF
        /// Parity - ?
        | PF
        /// Carry - Unsigned number exceeds capacity of result
        | CF
    
    let flagNames = 
        [ (OF, ("OV", "NV"))
          (DF, ("DN", "UP"))
          (IF, ("EI", "DI"))
          (TF, ("", ""))
          (SF, ("NG", "PL"))
          (ZF, ("ZR", "NZ"))
          (AF, ("AC", "NA"))
          (PF, ("PE", "PO"))
          (CF, ("CY", "NC")) ]
        |> Map.ofList
    
    type CPU = 
        { mutable AX : Word16
          mutable BX : Word16
          mutable CX : Word16
          mutable DX : Word16
          mutable SP : Word16
          mutable BP : Word16
          mutable SI : Word16
          mutable DI : Word16
          mutable IP : Word16
          mutable CS : Word16
          mutable DS : Word16
          mutable SS : Word16
          mutable ES : Word16
          Flags : Dictionary<Flags, bool>
          mutable Pending : bool
          mutable SegOverride : RegisterSeg option
          mutable RepType : RepetitionType option
          mutable ITicks : int64
          mutable ICount : int64 }
    
    type MemoryBlock<'a> = 'a array
    
    type Motherboard = 
        { SW : Stopwatch
          mutable CPU : CPU
          RAM : MemoryBlock<Word8>
          ReadOnly : MemoryBlock<bool>
          PortRAM : MemoryBlock<Word8> }
        override x.ToString() = 
            let l1 = 
                sprintf "AX=%04X  BX=%04X  CX=%04X  DX=%04X  SP=%04X  BP=%04X  SI=%04X  DI=%04X" x.CPU.AX x.CPU.BX 
                    x.CPU.CX x.CPU.DX x.CPU.SP x.CPU.BP x.CPU.SI x.CPU.DI
            let l2 = sprintf "DS=%04X  ES=%04X  SS=%04X  CS=%04X  IP=%04X" x.CPU.DS x.CPU.ES x.CPU.SS x.CPU.CS x.CPU.IP
            
            let fs = 
                x.CPU.Flags.Keys
                |> Seq.map (fun k -> 
                       flagNames
                       |> Map.find k
                       |> (if x.CPU.Flags.[k] then fst
                           else snd))
                |> String.concat " "
            sprintf "%s\n%s   %s" l1 l2 fs
