open Lib.Parser.Core
open Lib.Disassembler
open System
open System.Reflection
open System.IO
open Lib.Parser.TextInput

[<EntryPoint>]
let main argv = 

    let instrs = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, "8086_table.txt")
        |> File.ReadAllText
        |> Lib.InstructionSetLoader.loadInstructionSet

    let input = [|
        0xDDuy;
        0xB0uy; 0b11011000uy; 0xF0uy;
        0xDDuy;
        0xDDuy;
        0xDDuy;
        0x11uy; 0b00101110uy; 0xF0uy;
        0xDDuy;
        0xDDuy;
        0xDDuy;
        0xC4uy; 0b01000000uy; 0xF0uy;
        0xC5uy; 0b01000000uy; 0xF0uy;
        0xDDuy;
        0x22uy; 0xDEuy; 0xefuy; 0xdduy; 0x01uy; 0xC4uy; 0xDDuy; 0x71uy; 0x80uy; 0xaduy; 0xbauy; 0x0duy; 0xf0uy; 0xDDuy |] |> fromBytes

    let rec loop (s, o) input =
        match runOnInput (pinstruction (s, o) instrs) input with
        | Success(i, input) -> 
            printfn "%O" i
            loop input
        | Failure(pl, pe, pp) -> 
            printf "Parse failed: %A %A %A" pl pe pp
    loop (0us, 0us)
    input

    0 // return an integer exit code
