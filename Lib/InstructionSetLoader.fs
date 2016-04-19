module Lib.InstructionSetLoader

open Lib.Domain
open System
open System.Globalization

let private split ss (l : string) = 
    l.Split(ss)
    |> Array.filter (Seq.isEmpty >> not)
    |> Array.toList

let loadInstructionSet (text : string) = 
    let createOpce = 
        function 
        | x :: xs -> Byte.Parse(x, NumberStyles.HexNumber), xs
        | _ -> failwith "OpCode not in expected format"
    
    let newOce (s : string) = 
        match s |> split [| '/' |] with
        | [ g; i ] -> 
            { OcgName = g
              OcgIndex = Byte.Parse(i, NumberStyles.Integer) }
        | _ -> failwith "OpCode Group not in expected format"
    
    let createOpcxe = 
        function 
        | [] -> failwith "OpCode Group not in expected format"
        | x :: xs -> (newOce x, xs)
    
    let (opcxLines, opcLines) = 
        text.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.partition (fun s -> s.StartsWith("GRP"))
    
    let opc = 
        opcLines
        |> List.map (split [||] >> createOpce)
        |> Map.ofList
    
    let opcx = 
        opcxLines
        |> List.map (split [||] >> createOpcxe)
        |> Map.ofList
    
    { OpCodes = opc
      OpCodeGroups = opcx }
