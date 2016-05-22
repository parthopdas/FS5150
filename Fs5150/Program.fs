// return an integer exit code
module Program

open Lib.CPU
open Lib.Parser.Core
open System

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
            | _ -> "" |> Result.unit
        Console.ReadLine()
        |> execCmd
        |> printf "%O"
        loop()
    loop()
    0
