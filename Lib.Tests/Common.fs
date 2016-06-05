namespace Lib

module Common =
    open System
    open System.IO
    open System.Reflection

    let is = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, "8086_table.txt")
        |> File.ReadAllText
        |> Lib.CPU.InstructionSetLoader.loadInstructionSet

    let grammer = 
        is 
        |> Lib.CPU.InstructionSetLoader.loadGrammer
