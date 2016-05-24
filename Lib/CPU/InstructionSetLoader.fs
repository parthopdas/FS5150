namespace Lib.CPU

module InstructionSetLoader = 
    open FSharpx.Text
    open Lib.Domain.InstructionSet
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
            text
            |> Strings.toLines
            |> Seq.filter (Strings.isNullOrEmpty >> not)
            |> Seq.filter (Strings.startsWith "*" >> not)
            |> Seq.toList
            |> List.partition (Strings.startsWith "GRP")
        
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
    
    let instrLen instr : Word16 = uint16 instr.Bytes.Length
