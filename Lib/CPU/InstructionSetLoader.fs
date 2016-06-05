namespace Lib.CPU

module InstructionSetLoader = 
    open FSharpx.Functional
    open FSharpx.Text
    open Lib.Common
    open Lib.Domain.InstructionSet
    open System
    open System.Globalization
    
    let private split ss (l : string) = 
        l.Split(ss)
        |> Array.filter (Seq.isEmpty >> not)
        |> Array.toList
    
    let loadInstructionSet (text : string) = 
        let ocdFromList xs = 
            { OcName = xs |> Seq.head
              OcArgs = 
                  xs
                  |> Seq.skip 1
                  |> Seq.toArray }
        
        let createOpce = 
            function 
            | x :: xs -> Byte.Parse(x, NumberStyles.HexNumber), ocdFromList xs
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
            | x :: xs -> (newOce x, ocdFromList xs)
        
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
    
    let getOpcodeId oc = ocIndices |> Array.findIndex ((=) oc)
    let getOpcodeGroupId ocg = ocgIndices |> Array.findIndex ((=) ocg)
    
    let toOcArg a = 
        let xform = 
            function 
            | "1" -> "One"
            | "3" -> "Three"
            | "M" -> "Mem"
            | s when Strings.startsWith "e" s -> s.[1..]
            | s -> s.Replace("0", "Z").Replace("v", "w").ToUpper()
        match Strings.toUnionCase (a |> xform) with
        | Some uc -> uc |> OcaSpecial
        | None -> 
            match Enum.TryParse<NormalArgCode>(a
                                               |> xform
                                               |> Strings.toCharArray
                                               |> Array.map toStr
                                               |> String.concat ", ") with
            | true, ac -> OcaNormal(ac)
            | false, _ -> Prelude.undefined
    
    let loadGrammer (is : InstructionSet) = 
        let toRule d = 
            let oct = 
                if Strings.startsWith "GRP" d.OcName then OctExtension
                else OctNormal
            { OcType = oct
              OcId = 
                  match oct with
                  | OctNormal -> getOpcodeId d.OcName
                  | OctExtension -> getOpcodeGroupId d.OcName
              OcArgs = d.OcArgs |> Array.map toOcArg }
        
        let opc = 
            is.OpCodes
            |> Map.toList
            |> List.map fst
            |> List.sort
            |> List.fold (fun acc e -> is.OpCodes.[e] :: acc) []
            |> List.rev
            |> List.map toRule
            |> List.toArray
        
        let opcg = 
            is.OpCodeGroups
            |> Map.toList
            |> List.map (fun (ocg, d) -> (ocg.OcgIndex, getOpcodeGroupId ocg.OcgName), d)
            |> Map.ofList
        
        let initOpcg i j = opcg.[(uint8) i, j] |> toRule
        { OpcRules = opc
          OpcgRules = Array2D.init 8 6 initOpcg }
    
    let instrLen instr : Word16 = uint16 instr.Bytes.Length
