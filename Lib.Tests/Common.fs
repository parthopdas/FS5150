namespace Lib

module Common = 
    open FSharpx
    open FsUnit.Xunit
    open Lib.CPU.Execution.Common
    open Lib.CPU.I8088
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open Lib.Parser.Core
    open System
    open System.Collections.Generic
    open System.IO
    open System.Reflection
    
    let private is = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, "8086_table.txt")
        |> File.ReadAllText
        |> Lib.CPU.InstructionSetLoader.loadInstructionSet
    
    let grammer = is |> Lib.CPU.InstructionSetLoader.loadGrammer
    let mb = InitParams.Default |> initMotherBoard
    
    let onBits (v : Word8) = 
        let v = v - ((v >>> 1) &&& 0x55uy)
        let v = (v &&& 0x33uy) + ((v >>> 2) &&& 0x33uy)
        (((v + (v >>> 4)) &&& 0x0Fuy) * 0x01uy) >>> 24
    
    let createMB tn = 
        { RamSize = 0x1000
          PortRamSize = 0
          CS = 0us
          IP = 0x100us }
        |> initMotherBoard
        |> loadBinary (Path.combine "TestData" tn) 0x100 false
    
    let runTestFromCOMFile mb = 
        (mb, (Running, HashSet<_>()))
        |> Result.returnM
        |> execAllLogicalInstrs
    
    let verifyAfterTestFromCOMFile mb iCount tCount = 
        let count = 
            0x0us @|@ 0x14aus
            |> readWord16
            |> Prelude.flip State.eval mb
        count |> should equal tCount
        mb.CPU.IP |> should equal 0x14Eus
        mb.CPU.CS |> should equal 0x0us
        mb.CPU.ICount |> should equal iCount
