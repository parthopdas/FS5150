open Lib.Parser.Core
open Lib.Disassembler
open System
open System.Reflection
open System.IO
open Lib.Parser.TextInput
open Lib.Domain.InstructionSet
open Lib.Domain.PC
open Lib.Parser.Core.Result


(*

Goal: Step through Fetch-Decode-Execute of given block of memory
- CS:IP loops back
- Print out address, instruction, bytes of next instruction, 
- Print out next 6 bytes to be fetched
- Print out CPU state

- Initialize MotherBoard [CPU * Memory]         () -> Motherboard
- Run CPU loop
    - Fetch intruction                          Motherboard -> Address * InputState<Position>
    - Decode instruction                        Address -> InputState<Position> -> Result<Instruction, _>
    - Dump state                                Motherboard -> string
    [Await command]
    - Run intstruction (just update csip)       Instruction -> Motherboard -> Motherboard
*)

type DbgCommand = 
    | Step of AsyncReplyChannel<Result<string>>
    | Dump of AsyncReplyChannel<Result<string>>
    
type DbgAgent() =

    let mbProcBody (inbox: MailboxProcessor<DbgCommand>) =
        let rec loop (mb: Result<Motherboard>) = async { 
            let! command = inbox.Receive()
            let newState =
                match command with
                | Step(rc) ->
                    let mb' = mb >>= stepCPU
                    mb' >>= dumpMotherboard |> rc.Reply
                    mb'
                | Dump(rc) ->
                    mb >>= dumpMotherboard |> rc.Reply
                    mb
            return! loop newState  
        }

        () |> initMotherBoard |> returnR |> loop

    let mailboxProc = MailboxProcessor.Start mbProcBody

    member __.Post(command) = 
        mailboxProc.Post command

[<EntryPoint>]
let main _ = 

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
        0x22uy; 0xDEuy; 0xefuy; 0xdduy; 0x01uy; 0xC4uy; 0xDDuy; 0x71uy; 0x80uy; 0xaduy; 0xbauy; 0x0duy; 0xf0uy; 0xDDuy |] |> fromBytes { Position.Offset = 0 }

    let rec loop csip input =
        match runOnInput (pinstruction csip instrs) input with
        | Success(i, input) -> 
            printfn "%O" i
            loop csip input
        | Failure(pl, pe, pp) -> 
            printf "Parse failed: %A %A %A" pl pe pp
    loop { Segment = 0us; Offset = 0us } input

    0 // return an integer exit code
