namespace Lib.Domain

module PC = 
    open InstructionSet
    open System.Diagnostics
    open System
    open System.Collections.Specialized
    
    [<Flags>]
    type Flags = 
        /// Overflow - Signed number exceeds capacity of result
        | OF = 0b0000100000000000
        /// Direction - Set by user to indication direction of string instructions
        | DF = 0b0000010000000000
        /// Interrupt - Enable or disable hardware interrupts
        | IF = 0b0000001000000000
        /// Trap - Single step
        | TF = 0b0000000100000000
        /// Sign - Results sign bit from compare/substract ops
        | SF = 0b0000000010000000
        /// Zero - Result is zero from compare/substract ops
        | ZF = 0b0000000001000000
        /// Adjust - ?
        | AF = 0b0000000000010000
        /// Parity - ?
        | PF = 0b0000000000000100
        /// Carry - Unsigned number exceeds capacity of result
        | CF = 0b0000000000000001
    
    let flagNames = 
        [ (Flags.OF, ("OV", "NV"))
          (Flags.DF, ("DN", "UP"))
          (Flags.IF, ("EI", "DI"))
          (Flags.TF, ("", ""))
          (Flags.SF, ("NG", "PL"))
          (Flags.ZF, ("ZR", "NZ"))
          (Flags.AF, ("AC", "NA"))
          (Flags.PF, ("PE", "PO"))
          (Flags.CF, ("CY", "NC")) ]
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
          mutable Flags : BitVector32
          mutable LogicalInstrStart : Address
          mutable SegmentOverride : RegisterSeg option
          mutable RepetitionType : RepetitionType
          mutable Halted : bool 
          mutable ITicks : int64
          mutable ICount : int64 }
    
    type MemoryBlock<'a> = 'a array
    
    type Motherboard = 
        { SW : Stopwatch
          CPU : CPU
          RAM : MemoryBlock<Word8>
          ReadOnly : MemoryBlock<bool>
          PortRAM : MemoryBlock<Word8> }
        override x.ToString() = 
            let l1 = 
                sprintf "AX=%04X  BX=%04X  CX=%04X  DX=%04X  SP=%04X  BP=%04X  SI=%04X  DI=%04X" x.CPU.AX x.CPU.BX 
                    x.CPU.CX x.CPU.DX x.CPU.SP x.CPU.BP x.CPU.SI x.CPU.DI
            let l2 = sprintf "DS=%04X  ES=%04X  SS=%04X  CS=%04X  IP=%04X" x.CPU.DS x.CPU.ES x.CPU.SS x.CPU.CS x.CPU.IP
            
            let fs = 
                Enum.GetValues(typeof<Flags>)
                |> Seq.cast<Flags>
                |> Seq.rev
                |> Seq.map (fun k -> 
                       flagNames
                       |> Map.find k
                       |> (if x.CPU.Flags.[int(k)] then fst
                           else snd))
                |> String.concat " "
            sprintf "%s\n%s   %s" l1 l2 fs
