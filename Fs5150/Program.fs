// return an integer exit code
module Program

open Lib
open Lib.Domain.PC
open Lib.Parser.Core
open System

type DbgCommand = 
    | Step of AsyncReplyChannel<Result<string>>
    | Dump of AsyncReplyChannel<Result<string>>

type DbgAgent() = 
    
    let processor (inbox : MailboxProcessor<DbgCommand>) = 
        let rec loop (mb : Result<Motherboard>) = 
            async { 
                let! command = inbox.Receive()
                let newState = 
                    match command with
                    | Step(rc) -> 
                        let mb' = mb |> Result.bindR CPU.stepCPU
                        mb'
                        |> Result.bindR CPU.dumpMotherboard
                        |> rc.Reply
                        mb'
                    | Dump(rc) -> 
                        mb
                        |> Result.bindR CPU.dumpMotherboard
                        |> rc.Reply
                        mb
                return! loop newState
            }
        ()
        |> initMotherBoard
        |> Result.returnR
        |> loop
    
    let mailboxProc = MailboxProcessor.Start processor
    member __.Step() = mailboxProc.PostAndReply Step
    member __.Dump() = mailboxProc.PostAndReply Dump

[<EntryPoint>]
let main _ = 
    printfn "IBM 5150 Emulator. (c) 2016, Partho P. Das"
    let dbg = DbgAgent()
    printf "\n%A" (dbg.Dump())
    let rec loop() = 
        printf "\n[s, d] - "
        match Console.ReadKey().Key with
        | ConsoleKey.S -> printf "\n%A" (dbg.Step())
        | ConsoleKey.D -> printf "\n%A" (dbg.Dump())
        | _ -> ()
        loop()
    loop()
    0
