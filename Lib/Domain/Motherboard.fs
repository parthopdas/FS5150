namespace Lib.Domain

module PC = 
    open InstructionSet
    open System.IO
    open System.Reflection
    open System
    
    type CPU = 
        { AX : Word16
          BX : Word16
          CX : Word16
          DX : Word16
          SP : Word16
          BP : Word16
          SI : Word16
          DI : Word16
          mutable IP : Word16
          Flags : Word16
          mutable CS : Word16
          DS : Word16
          SS : Word16
          ES : Word16 }
    
    type MemoryBlock = Word8 array
    
    type Motherboard = 
        { CPU : CPU
          BIOS : MemoryBlock }
        override x.ToString() = 
            let l1 = sprintf "AX=%04X  BX=%04X  CX=%04X  DX=%04X  SP=%04X  BP=%04X  SI=%04X  DI=%04X" x.CPU.AX x.CPU.BX x.CPU.CX x.CPU.DX x.CPU.SP x.CPU.BP x.CPU.SI x.CPU.DI
            let l2 = sprintf "DS=%04X  ES=%04X  SS=%04X  CS=%04X  IP=%04X   NV UP EI PL NZ NA PO NC" x.CPU.DS x.CPU.ES x.CPU.SS x.CPU.CS x.CPU.IP
            sprintf "%s\n%s" l1 l2

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
          BIOS = (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
                |> Path.GetFullPath
                |> Path.GetDirectoryName
                |> fun p -> Path.Combine(p, "PCXTBIOS.BIN")
                |> File.ReadAllBytes }
