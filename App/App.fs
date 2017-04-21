// return an integer exit code
module Program

open System
open Lib.Chips.I8088.I8088Agent
open Lib.Parser.Core

(*
TODO: Incomplete implementations:
- Implement signed offset in dereference
- NOT writes back only if (reg > 1) && (reg < 4)
- With CPU-Test BIOS Video mode doesnt work
 *)

(*
TODO:
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
            | _ -> "" |> ParserResult.returnM
        Console.ReadLine()
        |> fun cmdStr -> if cmdStr.Trim() = "" then prevCmdStr else cmdStr
        |> Prelude.tee (execCmd >> printf "%O")
        |> loop 
    loop ""
    0

// F000:E1C6