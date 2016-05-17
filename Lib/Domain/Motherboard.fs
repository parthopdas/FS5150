namespace Lib.Domain

module PC = 
    open InstructionSet
    open System.IO
    open System.Reflection
    open System
    
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

          mutable OF : bool
          mutable DF : bool
          mutable IF : bool 
          mutable TF : bool 
          mutable SF : bool
          mutable ZF : bool
          mutable AF : bool
          mutable PF : bool
          mutable CF : bool
          
          mutable SegOverride : SegRegister option }
    
    type MemoryBlock = Word8 array
    
    type Motherboard = 
        { CPU : CPU
          RAM : MemoryBlock
          BIOS : MemoryBlock }
        override x.ToString() = 
            let l1 = sprintf "AX=%04X  BX=%04X  CX=%04X  DX=%04X  SP=%04X  BP=%04X  SI=%04X  DI=%04X" x.CPU.AX x.CPU.BX x.CPU.CX x.CPU.DX x.CPU.SP x.CPU.BP x.CPU.SI x.CPU.DI
            let l2 = sprintf "DS=%04X  ES=%04X  SS=%04X  CS=%04X  IP=%04X" x.CPU.DS x.CPU.ES x.CPU.SS x.CPU.CS x.CPU.IP
            let fs = sprintf "%s %s %s %s %s %s %s %s" (if x.CPU.OF then "OV" else "NV") (if x.CPU.DF then "DN" else "UP") (if x.CPU.IF then "EI" else "DI") (if x.CPU.SF then "NG" else "PL") (if x.CPU.ZF then "ZR" else "NZ") (if x.CPU.AF then "AC" else "NA") (if x.CPU.PF then "PE" else "PO") (if x.CPU.CF then "CY" else "NC")
            sprintf "%s\n%s   %s" l1 l2 fs

    let initMotherBoard() : Motherboard = 
        { CPU = 
              { AX = 0us
                BX = 3us
                CX = 1us
                DX = 2us
                SP = 0us
                BP = 0us
                SI = 0us
                DI = 0us
                IP = 0us
                CS = 0xFFFFus
                DS = 0us
                SS = 0us
                ES = 0us
                
                OF = false
                DF = false
                IF = false 
                TF = false 
                SF = false
                ZF = false
                AF = false
                PF = false
                CF = false

                SegOverride = None }
          RAM = Array.zeroCreate 0x100000
          BIOS = (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
                |> Path.GetFullPath
                |> Path.GetDirectoryName
                |> fun p -> Path.Combine(p, "PCXTBIOS.BIN")
                |> File.ReadAllBytes }
