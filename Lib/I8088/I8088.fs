namespace Lib.Chips.I8088

open InstructionSet
open System
open System.Collections
open System.Collections.Specialized
open System.Diagnostics

[<Flags>]
type Flags = 
    | OF = 0b0000100000000000
    | DF = 0b0000010000000000
    | IF = 0b0000001000000000
    | TF = 0b0000000100000000
    | SF = 0b0000000010000000
    | ZF = 0b0000000001000000
    | AF = 0b0000000000010000
    | PF = 0b0000000000000100
    | CF = 0b0000000000000001

type Registers = 
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

type I8088 = 
    { SW : Stopwatch
      Registers : Registers
      RAM : MemoryBlock<Word8>
      ReadOnly : BitArray
      PortRAM : MemoryBlock<Word8> }
    override x.ToString() = 
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

        let l1 = 
            sprintf "AX=%04X  BX=%04X  CX=%04X  DX=%04X  SP=%04X  BP=%04X  SI=%04X  DI=%04X" x.Registers.AX x.Registers.BX 
                x.Registers.CX x.Registers.DX x.Registers.SP x.Registers.BP x.Registers.SI x.Registers.DI
        let l2 = sprintf "DS=%04X  ES=%04X  SS=%04X  CS=%04X  IP=%04X" x.Registers.DS x.Registers.ES x.Registers.SS x.Registers.CS x.Registers.IP
        
        let fs = 
            Enum.GetValues(typeof<Flags>)
            |> Seq.cast<Flags>
            |> Seq.rev
            |> Seq.map (fun k -> 
                    flagNames
                    |> Map.find k
                    |> (if x.Registers.Flags.[int(k)] then fst
                        else snd))
            |> String.concat " "
        sprintf "%s\n%s   %s" l1 l2 fs
