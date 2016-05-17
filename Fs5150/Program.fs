// return an integer exit code
module Program

open FSharpx.Text
open FSharpx.Text.Regex.Compiled
open Lib
open Lib.Domain.InstructionSet
open Lib.Domain.PC
open Lib.Parser.Core
open System
open System.Globalization

type DbgCommand = 
    | Trace of AsyncReplyChannel<Result<string>>
    | Register of AsyncReplyChannel<Result<string>>
    | Dump of Address * AsyncReplyChannel<Result<string>>

type DbgAgent() = 
    
    let processor (inbox : MailboxProcessor<DbgCommand>) = 
        let rec loop (mb : Result<Motherboard>) = 
            async { 
                let! command = inbox.Receive()
                let newState = 
                    match command with
                    | Trace(rc) -> 
                        let mb' = mb |> Result.bind CPU.stepCPU
                        mb'
                        |> Result.bind CPU.dumpRegisters
                        |> rc.Reply
                        mb'
                    | Register(rc) -> 
                        mb
                        |> Result.bind CPU.dumpRegisters
                        |> rc.Reply
                        mb
                    | Dump(a, rc) -> 
                        mb
                        |> Result.bind (CPU.dumpMemory a)
                        |> rc.Reply
                        mb
                return! loop newState
            }
        ()
        |> initMotherBoard
        |> Result.unit
        |> loop
    
    let mailboxProc = MailboxProcessor.Start processor
    member __.Trace() = mailboxProc.PostAndReply Trace
    member __.Register() = mailboxProc.PostAndReply Register
    member __.Dump a = mailboxProc.PostAndReply(fun rc -> Dump(a, rc))

let (|TraceCmdFormat|_|) = Regex.tryMatch "t"
let (|RegisterCmdFormat|_|) = Regex.tryMatch "r"

let (|DumpCmdFormat|_|) input = 
    match Regex.tryMatch "d ([0-9a-f]{1,4}):([0-9a-f]{1,4})" input with
    | Some am -> 
        Some { Segment = UInt16.Parse(am.Groups.[0].Value, NumberStyles.HexNumber)
               Offset = UInt16.Parse(am.Groups.[1].Value, NumberStyles.HexNumber) }
    | None -> None

[<EntryPoint>]
let main _ = 
    printf "IBM 5150 Emulator. (c) 2016, Partho P. Das"
    let dbg = DbgAgent()
    
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
