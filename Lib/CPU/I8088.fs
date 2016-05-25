namespace Lib.CPU

module I8088 = 
    open FSharpx
    open FSharpx.Functional
    open FSharpx.Text
    open Lib
    open Lib.CPU.Execution.Common
    open Lib.CPU.Execution.FDE
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open Lib.Parser.Core
    open System
    open System.Collections.Generic
    open System.Globalization
    open System.IO
    open System.Reflection
    
    let initMotherBoard() : Motherboard = 
        { ExecutedCount = 0
          CPU = 
              { AX = 0us
                BX = 3us
                CX = 1us
                DX = 2us
                SP = 0us
                BP = 0us
                SI = 0us
                DI = 0us
                IP = 0us
                CS = 0xFFFFus
                DS = 0us
                SS = 0us
                ES = 0us
                Flags = 
                    [ (OF, false)
                      (DF, false)
                      (IF, false)
                      (TF, false)
                      (SF, false)
                      (ZF, false)
                      (AF, false)
                      (PF, false)
                      (CF, false) ]
                    |> List.fold (fun acc e -> 
                           acc.Add(fst e, snd e)
                           acc) (Dictionary<Flags, bool>())
                SegOverride = None }
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
                  
    /// stepCPU :: Motherboard -> Result<Motherboard> 
    let stepCPU mb = 
        mb
        |> State.eval fetchInstr
        ||> decodeInstr
        |> Result.map fst
        |> Result.bind (executeInstr >> Result.unit)
        // TODO: P2D: Is short circuting this early OK?
        |> Result.bind (fun s -> State.exec s mb |> Result.unit)
    
    /// dumpRegisters :: Motherboard -> Result<string>
    let dumpRegisters mb = 
        let toStr x = x.ToString()
        
        let rinstr = 
            mb
            |> State.eval fetchInstr
            ||> decodeInstr
            |> Result.map fst
            |> Result.map toStr
        
        let rmbstr = 
            mb
            |> toStr
            |> Result.unit
        
        Result.lift2 (sprintf "%s\n%s") rmbstr rinstr
    
    /// dumpMemory :: Address -> Motherboard -> Result<string>
    let dumpMemory addr mb = 
        let bytesToPrint = 128
        let bytesPerLine = 0x10
        
        let sbs = 
            [ for i in 0..(bytesToPrint - 1) do
                  yield incrAddress ((uint16) i) addr ]
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
                let a = incrAddress ((uint16) (eK * bytesPerLine)) addr
                
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
        |> Result.unit
    
    type I8088Command = 
        | Trace of AsyncReplyChannel<Result<string>>
        | Register of AsyncReplyChannel<Result<string>>
        | Dump of Address * AsyncReplyChannel<Result<string>>
    
    let (|TraceCmdFormat|_|) = Regex.tryMatch "t"
    let (|RegisterCmdFormat|_|) = Regex.tryMatch "r"
    
    let (|DumpCmdFormat|_|) input = 
        match Regex.tryMatch "d ([0-9a-f]{1,4}):([0-9a-f]{1,4})" input with
        | Some am -> 
            Some { Segment = UInt16.Parse(am.Groups.[0].Value, NumberStyles.HexNumber)
                   Offset = UInt16.Parse(am.Groups.[1].Value, NumberStyles.HexNumber) }
        | None -> None
    
    type I8088Agent() = 
        
        let processor (inbox : MailboxProcessor<I8088Command>) = 
            let rec nextCmd (mb, br) = 
                let continueWithBr = (Prelude.flip Prelude.tuple2 br) |> Result.map
                async { 
                    let! command = inbox.TryReceive(if br then -1
                                                    else 0)
                    let f = 
                        match command with
                        | Some(Trace(rc)) -> 
                            stepCPU
                            >> Common.tee (Result.bind dumpRegisters >> rc.Reply)
                            >> continueWithBr
                        | Some(Register(rc)) -> 
                            Common.tee (dumpRegisters >> rc.Reply)
                            >> Result.unit
                            >> continueWithBr
                        | Some(Dump(a, rc)) -> 
                            Common.tee (dumpMemory a >> rc.Reply)
                            >> Result.unit
                            >> continueWithBr
                        | None -> stepCPU >> Result.bind (fun mb -> (mb, mb.ExecutedCount = 55) |> Result.unit)
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
            |> Result.unit
            |> loop
        
        let mailboxProc = MailboxProcessor.Start processor
        member __.Trace() = mailboxProc.PostAndReply Trace
        member __.Register() = mailboxProc.PostAndReply Register
        member __.Dump a = mailboxProc.PostAndReply(fun rc -> Dump(a, rc))
