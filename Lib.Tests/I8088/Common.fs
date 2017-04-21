namespace Lib

module Common = 
    open FSharpx
    open FsUnit.Xunit
    open Lib.Chips.I8088.Execution.Common
    open Lib.Chips.I8088.I8088Agent
    open Lib.Chips.I8088.InstructionSet
    open Lib.Chips.I8088
    open Lib.Parser.Core
    open System
    open System.Collections.Generic
    open System.IO
    open System.Reflection
    
    let private is = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, @"I8088\8086_table.txt")
        |> File.ReadAllText
        |> InstructionSetLoader.loadInstructionSet
    
    let grammer = is |> InstructionSetLoader.loadGrammer
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
        |> loadBinary (Path.combine @"I8088\TestData" tn) 0x100 false
    
    let runTestFromCOMFile mb = 
        (mb, (Running, HashSet<_>()))
        |> ParserResult.returnM
        |> execAllLogicalInstrs
    
    let verifyAfterTestFromCOMFile mb (tCount: int) (iCount: int) = 
        let successExitIp = 0x163us
        let counterOff = 0x14eus
        let testStubICount = 18L
        let count = 
            0x0us @|@ counterOff
            |> readWord16
            |> Prelude.flip State.eval mb
        count |> should equal (Word16(tCount))
        mb.Registers.IP |> should equal (successExitIp + 1us)
        mb.Registers.CS |> should equal 0x0us
        mb.Registers.ICount |> should equal (int64(iCount) + testStubICount)
