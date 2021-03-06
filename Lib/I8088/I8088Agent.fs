﻿namespace Lib.Chips.I8088

module I8088Agent = 
    open YaFunTK
    open FSharpx
    open FSharpx.Text
    open Lib.Chips.I8088.Execution.Common
    open Lib.Chips.I8088.Execution.FDE
    open Lib.Chips.I8088.InstructionSet
    open Lib.Chips.I8088
    open Lib.Parser.Core
    open System
    open System.Diagnostics
    open System.Globalization
    open System.IO
    open System.Collections.Generic
    open System.Collections
    
    // TODO: PERF: State monad might be causing some bottlenecks. Consider a home grown implementation.

    type InitParams =
        { RamSize : int
          PortRamSize : int
          CS : Word16
          IP : Word16 }
        static member Default = 
            { RamSize = 0x100000
              PortRamSize = 0x10000
              CS = 0xFFFFus
              IP = 0us }

    type CpuLoopState =
        | Running
        | Paused
        | Halted

    let initMotherBoard init : I8088 = 
        { SW = Stopwatch()
          Registers = { initialCPU() with CS = init.CS; IP = init.IP } 
          RAM = Array.create init.RamSize 0xfeuy
          ReadOnly = BitArray(init.RamSize)
          PortRAM = Array.zeroCreate init.PortRamSize }
    
    let loadBinary fname addr ro mb = 
        (Path.getLocalPath(), fname)
        ||> Path.combine
        |> File.ReadAllBytes
        |> (fun bs -> 
            Array.blit bs 0 mb.RAM addr bs.Length
            bs.Length)
        |> (fun len -> 
            let a = Array.zeroCreate<bool> mb.RAM.Length
            Array.fill a addr len ro
            mb.ReadOnly.Or(BitArray(a)) |> ignore)
        mb
    
    let getStats (mb, _) = 
        sprintf 
            "[Timer: %s] Count = %d, Ticks = %d, Average = %.6fmips" 
            (if Stopwatch.IsHighResolution && not mb.SW.IsRunning then "OK" else "NOT OK") 
            mb.Registers.ICount 
            mb.Registers.ITicks
            (((float)Stopwatch.Frequency) / ((float)mb.Registers.ITicks / (float)mb.Registers.ICount) / 1000000.0)
        |> ParserResult.returnM
    
    let execOneLogicalInstr (mb, (ls, bps : HashSet<_>)) = 
        let execPhysicalInstr mb = 
            mb
            |> State.eval (State.( *> ) beforePhysicalInstr fetchInstr)
            |> Prelude.uncurry decodeInstr
            |> ParserResult.bind (fun is -> 
                            let i = is |> fst
                            let s = State.( <* ) (i |> executeInstr) afterPhysicalInstr
                            (State.exec s mb, i.IsPrefix) |> ParserResult.returnM)
    
        let rec loopExecPrefixInstrs (mb, (ls, bps)) =
            mb
            |> execPhysicalInstr
            |> ParserResult.bind (fun (mb',isPrefix) -> 
                            if isPrefix then 
                                loopExecPrefixInstrs (mb', (ls, bps)) 
                            else (ParserResult.returnM (mb', (ls, bps))))
        if mb.Registers.Halted then
            (mb, (Halted, bps)) |> ParserResult.returnM
        else if ls = Running && mb.Registers.CS @|@ mb.Registers.IP |> bps.Contains then
            (mb, (Paused, bps)) |> ParserResult.returnM
        else
            (mb |> State.exec beforeLogicalInstr, (ls, bps))
            |> loopExecPrefixInstrs 

    let rec private execNextLogicalInstr (mb, state) = 
        if not mb.Registers.Halted then
            (mb, state)
            |> execOneLogicalInstr
            |> execAllLogicalInstrs
    and execAllLogicalInstrs = Result.fold execNextLogicalInstr ignore
    
    let dumpRegisters (mb, _) = 
        let rinstr = 
            mb
            |> State.eval fetchInstr
            ||> decodeInstr
            |> ParserResult.map fst
            |> ParserResult.map Prelude.toStr
        
        let rmbstr = 
            mb
            |> Prelude.toStr
            |> ParserResult.returnM
        
        ParserResult.lift2 (sprintf "%s\n%s") rmbstr rinstr
    
    let setBreakPoint addr (_, (ls, bps : HashSet<_>)) =
        bps.Add(addr) |> ignore
        let str = 
            bps 
            |> Seq.map Prelude.toStr
            |> Seq.sort
            |> String.concat Environment.NewLine
        ((ls, bps), str) |> ParserResult.returnM

    let dumpMemory addr (mb, _) = 
        let bytesToPrint = 128us
        let bytesPerLine = 0x10
        
        let sbs = 
            [ for i in 0us..(bytesToPrint - 1us) do
                  yield addr |++ i ]
            |> List.map readWord8
            |> State.sequence
        
        let l1 = 
            mb
            |> State.eval sbs
            |> List.mapi (fun i e -> i / bytesPerLine, e)
        
        let m1 = 
            List.foldBack (fun (eK, eV) acc -> 
                let m, l = 
                    acc
                    |> Map.tryFind eK
                    |> Option.fold (fun _ e -> (Map.remove eK acc), e) (acc, [])
                m |> Map.add eK (eV :: l)) l1 Map.empty
        
        let lines = 
            Map.foldBack (fun eK eT acc -> 
                let a = addr |++ (Word16(eK * bytesPerLine))
                
                let bstr = 
                    eT
                    |> List.map (sprintf "%02x ")
                    |> String.concat ""
                
                let pstr = 
                    eT
                    |> List.map (fun w -> 
                           let c = (char) w
                           if Char.IsControl(c) then "."
                           else c.ToString())
                    |> String.concat ""
                
                (sprintf "%O %O %O" a bstr pstr) :: acc) m1 []
        
        lines
        |> Strings.joinLines
        |> ParserResult.returnM
    
    let unassemble (mb, _) = 
        let rec getInstrAt ith is n = 
            getCSIP
            |> State.bind (Prelude.flip (|++) n >> createInputAt)
            |> State.bind (Prelude.uncurry decodeInstr >> State.returnM)
            |> Prelude.flip State.eval mb
            |> ParserResult.map fst
            |> ParserResult.bind (fun i -> 
                   if ith = 0 then i :: is |> ParserResult.returnM
                   else getInstrAt (ith - 1) (i :: is) (n + i.Length))
        getInstrAt 9 [] 0us |> ParserResult.bind (List.rev
                                            >> List.map Prelude.toStr
                                            >> Strings.joinLines
                                            >> ParserResult.returnM)
    
    type I8088Command = 
        | Break of AsyncReplyChannel<ParserResult<string>>
        | Resume of AsyncReplyChannel<ParserResult<string>>
        | SetBreakPoint of Address * AsyncReplyChannel<ParserResult<string>>
        | Dump of Address * AsyncReplyChannel<ParserResult<string>>
        | Register of AsyncReplyChannel<ParserResult<string>>
        | Stats of AsyncReplyChannel<ParserResult<string>>
        | Trace of AsyncReplyChannel<ParserResult<string>>
        | Unassemble of AsyncReplyChannel<ParserResult<string>>
    
    let (|BreakCmdFormat|_|) = Regex.tryMatch "^b$"
    let (|ResumeCmdFormat|_|) = Regex.tryMatch "^g$"
    let (|SetBreakPointCmdFormat|_|) input = 
        match Regex.tryMatch "^bp ([0-9a-fA-F]{1,4}):([0-9a-fA-F]{1,4})$" input with
        | Some am -> 
            Some { Segment = UInt16.Parse(am.Groups.[0].Value, NumberStyles.HexNumber)
                   Offset = UInt16.Parse(am.Groups.[1].Value, NumberStyles.HexNumber) }
        | None -> None
    let (|DumpCmdFormat|_|) input = 
        match Regex.tryMatch "^d ([0-9a-fA-F]{1,4}):([0-9a-fA-F]{1,4})$" input with
        | Some am -> 
            Some { Segment = UInt16.Parse(am.Groups.[0].Value, NumberStyles.HexNumber)
                   Offset = UInt16.Parse(am.Groups.[1].Value, NumberStyles.HexNumber) }
        | None -> None
    let (|RegisterCmdFormat|_|) = Regex.tryMatch "^r$"
    let (|StatsCmdFormat|_|) = Regex.tryMatch "^stats$"
    let (|TraceCmdFormat|_|) = Regex.tryMatch "^t$"    
    let (|UnassembleCmdFormat|_|) = Regex.tryMatch "^u$"
    
    type I8088Agent() = 
        
        let processor (inbox : MailboxProcessor<I8088Command>) = 
            let rec nextCmd (mb, (br, bps)) = 
                async {
                    let! command = inbox.TryReceive(match br with | Paused -> -1 | _ -> 0)
                    let f = 
                        match command with
                        | Some(Break(rc)) ->
                            rc.Reply("" |> ParserResult.returnM) 
                            fun (mb, (_, bps)) -> (mb, (Paused, bps)) |> ParserResult.returnM
                        | Some(Resume(rc)) ->
                            rc.Reply("" |> ParserResult.returnM) 
                            fun (mb, (_, bps)) -> (mb, (Running, bps)) |> ParserResult.returnM
                        | Some(SetBreakPoint(a, rc)) -> 
                                setBreakPoint a 
                                >> ParserResult.bind (
                                    Prelude.tee (snd >> ParserResult.returnM >> rc.Reply) 
                                    >> (fst >> Prelude.tuple2 mb >> ParserResult.returnM))
                        | Some(Stats(rc)) ->
                            Prelude.tee (getStats >> rc.Reply)
                            >> ParserResult.returnM 
                        | Some(Trace(rc)) -> 
                            execOneLogicalInstr
                            >> Prelude.tee (ParserResult.bind dumpRegisters >> rc.Reply)
                        | Some(Register(rc)) -> 
                            Prelude.tee (dumpRegisters >> rc.Reply)
                            >> ParserResult.returnM
                        | Some(Dump(a, rc)) -> 
                            Prelude.tee (dumpMemory a >> rc.Reply)
                            >> ParserResult.returnM
                        | Some(Unassemble(rc)) -> 
                            Prelude.tee (unassemble >> rc.Reply)
                            >> ParserResult.returnM
                        | None -> 
                            execOneLogicalInstr
                    return! (mb, (br, bps))
                            |> f
                            |> loop
                }
            
            and loop = Result.fold nextCmd (fun _ -> async { return () })

            InitParams.Default
            |> initMotherBoard
            |> loadBinary "PCXTBIOS.BIN" 0xFE000 true
            |> loadBinary "VIDEOROM.BIN" 0xC0000 true
            |> loadBinary "ROMBASIC.BIN" 0xF6000 false
            |> Prelude.tuple2 (Paused, HashSet<_>())
            |> Prelude.swap
            |> ParserResult.returnM
            |> loop
        
        let mailboxProc = MailboxProcessor.Start processor
        member __.Break() = mailboxProc.PostAndReply Break
        member __.Resume() = mailboxProc.PostAndReply Resume
        member __.SetBreakPoint a = mailboxProc.PostAndReply(fun rc -> SetBreakPoint(a, rc))
        member __.Dump a = mailboxProc.PostAndReply(fun rc -> Dump(a, rc))
        member __.Register() = mailboxProc.PostAndReply Register
        member __.Stats() = mailboxProc.PostAndReply Stats
        member __.Trace() = mailboxProc.PostAndReply Trace
        member __.Unassemble() = mailboxProc.PostAndReply Unassemble
