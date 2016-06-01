// return an integer exit code
module Program

open System
open Lib.CPU.I8088
open Lib.Parser.Core

(*
 Incomplete implementations:
 - Implement signed offset in dereference
 - NOT writes back only if (reg > 1) && (reg < 4)
 - With CPU-Test BIOS Video mode doesnt work
 - Interrupts cannot arrive between pairs of instruction like pop CS, etc.
 *)

[<EntryPoint>]
let main _ = 
    printf "IBM 5150 Emulator. (c) 2016, Partho P. Das"
    let dbg = I8088Agent()
    
    let rec loop() = 
        printf "\n-"
        let execCmd = 
            function 
            | TraceCmdFormat _ -> dbg.Trace()
            | RegisterCmdFormat _ -> dbg.Register()
            | DumpCmdFormat a -> dbg.Dump(a)
            | UnassembleCmdFormat _ -> dbg.Unassemble()
            | _ -> "" |> Result.returnM
        Console.ReadLine()
        |> execCmd
        |> printf "%O"
        loop()
    loop()
    0
