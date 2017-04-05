module Lib.Excution.Arithmetic.Tests

open YaFunTK
open FSharpx
open FsCheck
open Lib
open Lib.CPU.Execution.Arithmetic
open Lib.CPU.Execution.Common
open Lib.CPU.I8088
open Lib.Parser.Core
open Lib.Domain.InstructionSet
open Lib.Domain.PC
open System
open global.Xunit
open FsUnit.Xunit

[<Fact>]
let ``ADD Tests``() = 
    let mb =
        { RamSize = 0x1000; PortRamSize = 0; CS = 0us; IP = 0x100us }
        |> initMotherBoard
        |> loadBinary (Path.combine "TestData" "ADD.tests.com") 0x100 false

    let rec nextCmd mb = 
        mb |> execLogicalInstr |> loop
    and loop = Result.fold nextCmd ignore

    mb |> Result.returnM |> loop

    mb.CPU.ICount |> should equal 0x49L
    mb.CPU.CS |> should equal 0x0us
    mb.CPU.IP |> should equal 0x107us
    0x0us @|@ 0x103us |> readWord16 |> Prelude.flip State.eval mb |> should equal 14us

module SUB = 
    let fSubRes8 = (fun a1 a2 -> a1 + (~~~a2 + 1uy))
    let fMSBit8 = (Prelude.flip (>>>) 7 >> int)
    let fOnBits8 = (Common.onBits >> int)
    let fInURange8 = (fun a1 a2 -> uint16 (a1) - uint16 (a2) > 0xFFus)
    let fInSRange8 = 
        (fun a1 a2 -> 
        (int16 (int8 (a1)) - int16 (int8 (a2)) > (int16) SByte.MaxValue 
         || int16 (int8 (a1)) - int16 (int8 (a2)) < (int16) SByte.MinValue))
    let fLSNibble8 = (fun a1 a2 -> (a1 &&& 0x0Fuy) - (a2 &&& 0x0Fuy) |> int)
    let fSubRes16 = (fun a1 a2 -> a1 + (~~~a2 + 1us))
    let fMSBit16 = (Prelude.flip (>>>) 15 >> int)
    
    let fOnBits16 = 
        ((!><)
         >> Common.onBits
         >> int)
    
    let fInURange16 = (fun a1 a2 -> uint32 (a1) - uint32 (a2) > 0xFFFFu)
    let fInSRange16 = 
        (fun a1 a2 -> 
        (int (int16 (a1)) - int (int16 (a2)) > (int) Int16.MaxValue 
         || int (int16 (a1)) - int (int16 (a2)) < (int) Int16.MinValue))
    let fLSNibble16 = (fun a1 a2 -> (a1 &&& 0x000Fus) - (a2 &&& 0x000Fus) |> int)
    
    let lawCore f fRes (fMsb, fOnBits, fInURange, fInSRange, fLSNibble) a1 a2 = 
        let mb = Common.mb
        let res = State.eval (f (a1, a2)) mb
        res = fRes a1 a2 && mb.CPU.Flags.[int(Flags.ZF)] = (a1 = a2) && mb.CPU.Flags.[int(Flags.SF)] = (fMsb res = 1) 
        && mb.CPU.Flags.[int(Flags.PF)] = (fOnBits res % 2 = 0) && mb.CPU.Flags.[int(Flags.CF)] = (fInURange a1 a2) 
        && mb.CPU.Flags.[int(Flags.OF)] = fInSRange a1 a2 && mb.CPU.Flags.[int(Flags.AF)] = (fLSNibble a1 a2 > 0xF)
    
    [<Xunit.Fact>]
    let ``Core SUB 8``() = 
        let law (a1 : Word8) (a2 : Word8) = 
            lawCore coreSUB8 fSubRes8 (fMSBit8, fOnBits8, fInURange8, fInSRange8, fLSNibble8) a1 a2
        Check.QuickThrowOnFailure law
    
    [<Xunit.Fact>]
    let ``Core SUB 16``() = 
        let law (v1H : Word8) (v1L : Word8) (v2H : Word8) (v2L : Word8) = 
            let a1 = v1H +|+ v1L
            let a2 = v2H +|+ v2L
            lawCore coreSUB16 fSubRes16 (fMSBit16, fOnBits16, fInURange16, fInSRange16, fLSNibble16) a1 a2
        Check.QuickThrowOnFailure law
