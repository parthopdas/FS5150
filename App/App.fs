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

(*
 - Move away from strings in op code parsing
 - Move away from maps in op code parsing
 - Unnecessary data is conditional inpinstruction
 *)

[<EntryPoint>]
let main _ = 
    printf "IBM 5150 Emulator. (c) 2016, Partho P. Das"
    let dbg = I8088Agent()
    
    let rec loop prevCmdStr = 
        printf "\n-"
        let execCmd = 
            function 
            | BreakCmdFormat _ -> dbg.Break()
            | ResumeCmdFormat _ -> dbg.Resume()
            | SetBreakPointCmdFormat a -> dbg.SetBreakPoint(a)
            | DumpCmdFormat a -> dbg.Dump(a)
            | RegisterCmdFormat _ -> dbg.Register()
            | StatsCmdFormat _ -> dbg.Stats()
            | TraceCmdFormat _ -> dbg.Trace()
            | UnassembleCmdFormat _ -> dbg.Unassemble()
            | _ -> "" |> Result.returnM
        Console.ReadLine()
        |> fun cmdStr -> if cmdStr.Trim() = "" then prevCmdStr else cmdStr
        |> Prelude.tee (execCmd >> printf "%O")
        |> loop 
    loop ""
    0

// F000:E196