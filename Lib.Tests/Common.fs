namespace Lib

module Common =
    open System
    open System.IO
    open System.Reflection
    open Lib.Domain.InstructionSet
    open Lib.CPU.I8088

    let private is = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, "8086_table.txt")
        |> File.ReadAllText
        |> Lib.CPU.InstructionSetLoader.loadInstructionSet

    let grammer = 
        is 
        |> Lib.CPU.InstructionSetLoader.loadGrammer

    let mb = InitParams.Default |> initMotherBoard

    let onBits (v : Word8) =
        let v = v - ((v >>> 1) &&& 0x55uy);
        let v = (v &&& 0x33uy) + ((v >>> 2) &&& 0x33uy);
        (((v + (v >>> 4)) &&& 0x0Fuy) * 0x01uy) >>> 24;
