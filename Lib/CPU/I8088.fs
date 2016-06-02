namespace Lib.CPU

module I8088 = 
    open FSharpx
    open FSharpx.Functional
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
    
    let initMotherBoard() : Motherboard = 
        { SW = new Stopwatch()
          CPU = initialCPU()
          RAM = Array.zeroCreate 0x100000
          ReadOnly = Array.zeroCreate 0x100000
          PortRAM = Array.zeroCreate 0x10000 }
    
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
    
    /// logicalStepCPU :: Motherboard -> Result<Motherboard> 
    let logicalStepCPU mb = 
        let rec physicalStepCPU mb = 
            mb
            |> State.eval (State.( *> ) beforePhysicalInstruction createInputAtCSIP)
            |> Prelude.uncurry decodeInstr
            |> Result.map fst
            |> Result.bind (fun i -> 
                            let s = State.( <* ) (i |> executeInstr) afterPhysicalInstruction
                            let mb' = s |> Prelude.flip State.exec mb
                            if mb'.CPU.Pending then (physicalStepCPU mb') else (Result.returnM mb'))
        
        let reset = 
            mb
            |> State.eval beforeLogicalInstruction
            |> Result.returnM
        Result.( *> ) reset (physicalStepCPU mb)
    
    /// dumpRegisters :: Motherboard -> Result<string>
    let dumpRegisters mb = 
        let rinstr = 
            mb
            |> State.eval createInputAtCSIP
            ||> decodeInstr
            |> Result.map fst
            |> Result.map toStr
        
        let rmbstr = 
            mb
            |> toStr
            |> Result.returnM
        
        Result.lift2 (sprintf "%s\n%s") rmbstr rinstr
    
    /// dumpMemory :: Address -> Motherboard -> Result<string>
    let dumpMemory addr mb = 
        let bytesToPrint = 128us
        let bytesPerLine = 0x10
        
        let sbs = 
            [ for i in 0us..(bytesToPrint - 1us) do
                  yield i |++ addr ]
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
                let a = ((uint16) (eK * bytesPerLine)) |++ addr
                
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
            |> State.bind (fun a -> n |++ a |> createInputAt)
            |> State.bind (Prelude.uncurry decodeInstr >> State.returnM)
            |> Prelude.flip State.eval mb
            |> Result.map fst
            |> Result.bind (fun i -> 
                   if ith = 0 then i :: is |> Result.returnM
                   else getInstrAt (ith - 1) (i :: is) (n + i.Length))
        getInstrAt 9 [] 0us |> Result.bind (List.rev
                                            >> List.map toStr
                                            >> Strings.joinLines
                                            >> Result.returnM)
    
    type I8088Command = 
        | Stats of AsyncReplyChannel<Result<string>>
        | Trace of AsyncReplyChannel<Result<string>>
        | Register of AsyncReplyChannel<Result<string>>
        | Dump of Address * AsyncReplyChannel<Result<string>>
        | Unassemble of AsyncReplyChannel<Result<string>>
    
    let (|StatsCmdFormat|_|) = Regex.tryMatch "^stats$"
    let (|TraceCmdFormat|_|) = Regex.tryMatch "^t$"
    let (|RegisterCmdFormat|_|) = Regex.tryMatch "^r$"
    
    let (|DumpCmdFormat|_|) input = 
        match Regex.tryMatch "^d ([0-9a-f]{1,4}):([0-9a-f]{1,4})$" input with
        | Some am -> 
            Some { Segment = UInt16.Parse(am.Groups.[0].Value, NumberStyles.HexNumber)
                   Offset = UInt16.Parse(am.Groups.[1].Value, NumberStyles.HexNumber) }
        | None -> None
    
    let (|UnassembleCmdFormat|_|) = Regex.tryMatch "u"
    
    type I8088Agent() = 
        
        let processor (inbox : MailboxProcessor<I8088Command>) = 
            let rec nextCmd (mb, br) = 
                let continueWithBr = (Prelude.flip Prelude.tuple2 br) |> Result.map
                async { 
                    let! command = inbox.TryReceive(if br then -1
                                                    else 0)
                    let f = 
                        match command with
                        | Some(Stats(rc)) -> 
                            (fun mb -> mb |> Result.returnM)
                            >> Common.tee (Result.bind getStats >> rc.Reply)
                            >> continueWithBr
                        | Some(Trace(rc)) -> 
                            logicalStepCPU
                            >> Common.tee (Result.bind dumpRegisters >> rc.Reply)
                            >> continueWithBr
                        | Some(Register(rc)) -> 
                            Common.tee (dumpRegisters >> rc.Reply)
                            >> Result.returnM
                            >> continueWithBr
                        | Some(Dump(a, rc)) -> 
                            Common.tee (dumpMemory a >> rc.Reply)
                            >> Result.returnM
                            >> continueWithBr
                        | Some(Unassemble(rc)) -> 
                            Common.tee (unassemble >> rc.Reply)
                            >> Result.returnM
                            >> continueWithBr
                        | None -> 
                            logicalStepCPU >> Result.bind (fun mb -> 
                                                  let x = 
                                                      if mb.CPU.ICount % 105L = 0L then resetCPUState
                                                      else Result.returnM
                                                  mb
                                                  |> x
                                                  |> Result.bind (fun mb -> (mb, false) |> Result.returnM))
                    return! mb
                            |> f
                            |> loop
                }
            
            and loop = Result.fold nextCmd (fun _ -> async { return () })
            ()
            |> initMotherBoard
            |> loadBinary "PCXTBIOS.BIN" 0xFE000 true
            |> loadBinary "VIDEOROM.BIN" 0xC0000 true
            |> loadBinary "ROMBASIC.BIN" 0xF6000 false
            |> Prelude.tuple2 false
            |> Prelude.swap
            |> Result.returnM
            |> loop
        
        let mailboxProc = MailboxProcessor.Start processor
        member __.Stats() = mailboxProc.PostAndReply Stats
        member __.Trace() = mailboxProc.PostAndReply Trace
        member __.Register() = mailboxProc.PostAndReply Register
        member __.Dump a = mailboxProc.PostAndReply(fun rc -> Dump(a, rc))
        member __.Unassemble() = mailboxProc.PostAndReply Unassemble
