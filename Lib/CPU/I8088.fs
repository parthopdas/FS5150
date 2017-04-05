namespace Lib.CPU

module I8088 = 
    open YaFunTK
    open FSharpx
    open FSharpx.Text
    open Lib
    open Lib.CPU.Execution.Common
    open Lib.CPU.Execution.FDE
    open Lib.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open Lib.Parser.Core
    open System
    open System.Diagnostics
    open System.Globalization
    open System.IO
    open System.Reflection
    
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

    let initMotherBoard init : Motherboard = 
        { SW = Stopwatch()
          CPU = { initialCPU() with CS = init.CS; IP = init.IP } 
          RAM = Array.create init.RamSize 0xfeuy
          ReadOnly = Array.zeroCreate init.RamSize
          PortRAM = Array.zeroCreate init.PortRamSize }
    
    let loadBinary fname addr ro mb = 
        (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        |> Path.GetFullPath
        |> Path.GetDirectoryName
        |> fun p -> Path.Combine(p, fname)
        |> File.ReadAllBytes
        |> (fun bs -> Array.blit bs 0 mb.RAM addr bs.Length; bs.Length)
        |> (fun len -> Array.fill mb.ReadOnly addr len ro)
        mb
    
    let getStats mb = 
        sprintf 
            "[Timer: %s] Count = %d, Ticks = %d, Average = %.6fmips" 
            (if Stopwatch.IsHighResolution && not mb.SW.IsRunning then "OK" else "NOT OK") 
            mb.CPU.ICount 
            mb.CPU.ITicks
            (((float)Stopwatch.Frequency) / ((float)mb.CPU.ITicks / (float)mb.CPU.ICount) / 1000000.0)
        |> Result.returnM
    
    let resetCPUState = State.exec resetCPU >> Result.returnM

    let execLogicalInstr mb = 
        let execPhysicalInstr mb = 
            mb
            |> State.eval (State.( *> ) beforePhysicalInstr createInputAtCSIP)
            |> Prelude.uncurry decodeInstr
            |> Result.bind (fun is -> 
                            let i = is |> fst
                            let s = State.( <* ) (i |> executeInstr) afterPhysicalInstr
                            (State.exec s mb, i.IsPrefix) |> Result.returnM)
    
        let rec loopExecPrefixInstrs mb =
            mb
            |> execPhysicalInstr
            |> Result.bind (fun (mb',isPrefix) -> 
                            if isPrefix then 
                                loopExecPrefixInstrs mb' 
                            else (Result.returnM mb'))
        if mb.CPU.Halted then
            Failure("CPU Halted", EndOfInput, { CurrentBytes = Array.empty; CurrentOffset = mb.CPU.CS @|@ mb.CPU.IP |> flatten |> int })
        else
            mb
            |> State.exec beforeLogicalInstr
            |> loopExecPrefixInstrs
    
    /// dumpRegisters :: Motherboard -> Result<string>
    let dumpRegisters mb = 
        let rinstr = 
            mb
            |> State.eval createInputAtCSIP
            ||> decodeInstr
            |> Result.map fst
            |> Result.map Prelude.toStr
        
        let rmbstr = 
            mb
            |> Prelude.toStr
            |> Result.returnM
        
        Result.lift2 (sprintf "%s\n%s") rmbstr rinstr
    
    /// dumpMemory :: Address -> Motherboard -> Result<string>
    let dumpMemory addr mb = 
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
                let a = addr |++ ((uint16) (eK * bytesPerLine))
                
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
        |> Result.returnM
    
    /// unassemble :: Motherboard -> Result<string>
    let unassemble mb = 
        let rec getInstrAt ith is n = 
            getCSIP
            |> State.bind (Prelude.flip (|++) n >> createInputAt)
            |> State.bind (Prelude.uncurry decodeInstr >> State.returnM)
            |> Prelude.flip State.eval mb
            |> Result.map fst
            |> Result.bind (fun i -> 
                   if ith = 0 then i :: is |> Result.returnM
                   else getInstrAt (ith - 1) (i :: is) (n + i.Length))
        getInstrAt 9 [] 0us |> Result.bind (List.rev
                                            >> List.map Prelude.toStr
                                            >> Strings.joinLines
                                            >> Result.returnM)
    
    type I8088Command = 
        | Break of AsyncReplyChannel<Result<string>>
        | Dump of Address * AsyncReplyChannel<Result<string>>
        | Register of AsyncReplyChannel<Result<string>>
        | Stats of AsyncReplyChannel<Result<string>>
        | Trace of AsyncReplyChannel<Result<string>>
        | Unassemble of AsyncReplyChannel<Result<string>>
    
    let (|BreakCmdFormat|_|) = Regex.tryMatch "^b$"
    let (|DumpCmdFormat|_|) input = 
        match Regex.tryMatch "^d ([0-9a-f]{1,4}):([0-9a-f]{1,4})$" input with
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
            let rec nextCmd (mb, br) = 
                let continueWithBr = (Prelude.flip Prelude.tuple2 br) |> Result.map
                async { 
                    let! command = inbox.TryReceive(if br then -1
                                                    else 0)
                    let f = 
                        match command with
                        | Some(Break(rc)) ->
                            rc.Reply("" |> Result.returnM) 
                            fun mb -> (mb, true) |> Result.returnM
                        | Some(Stats(rc)) -> 
                            (fun mb -> mb |> Result.returnM)
                            >> Prelude.tee (Result.bind getStats >> rc.Reply)
                            >> continueWithBr
                        | Some(Trace(rc)) -> 
                            execLogicalInstr
                            >> Prelude.tee (Result.bind dumpRegisters >> rc.Reply)
                            >> continueWithBr
                        | Some(Register(rc)) -> 
                            Prelude.tee (dumpRegisters >> rc.Reply)
                            >> Result.returnM
                            >> continueWithBr
                        | Some(Dump(a, rc)) -> 
                            Prelude.tee (dumpMemory a >> rc.Reply)
                            >> Result.returnM
                            >> continueWithBr
                        | Some(Unassemble(rc)) -> 
                            Prelude.tee (unassemble >> rc.Reply)
                            >> Result.returnM
                            >> continueWithBr
                        | None -> 
                            execLogicalInstr >> Result.bind (fun mb -> 
                                                  let icount = 2L * 8192L + 110L
                                                  let x = 
                                                      //if mb.CPU.ICount % icount = 0L then resetCPUState
                                                      //else Result.returnM
                                                      Result.returnM
                                                  mb
                                                  |> x
                                                  |> Result.bind (fun mb -> (mb, mb.CPU.ICount > 655999L + 2048L + 2050L) |> Result.returnM))
                    return! mb
                            |> f
                            |> loop
                }
            
            and loop = Result.fold nextCmd (fun _ -> async { return () })

            InitParams.Default
            |> initMotherBoard
            |> loadBinary "PCXTBIOS.BIN" 0xFE000 true
            |> loadBinary "VIDEOROM.BIN" 0xC0000 true
            |> loadBinary "ROMBASIC.BIN" 0xF6000 false
            |> Prelude.tuple2 false
            |> Prelude.swap
            |> Result.returnM
            |> loop
        
        let mailboxProc = MailboxProcessor.Start processor
        member __.Break() = mailboxProc.PostAndReply Break
        member __.Dump a = mailboxProc.PostAndReply(fun rc -> Dump(a, rc))
        member __.Register() = mailboxProc.PostAndReply Register
        member __.Stats() = mailboxProc.PostAndReply Stats
        member __.Trace() = mailboxProc.PostAndReply Trace
        member __.Unassemble() = mailboxProc.PostAndReply Unassemble
