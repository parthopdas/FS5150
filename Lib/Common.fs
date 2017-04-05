namespace Lib

module Common =
    open System.Diagnostics

    // YoLo
    let inline dprintfn fmt = Printf.ksprintf Debug.WriteLine fmt
    
    // YoLo
    let inline tee fn x = x |> fn |> ignore; x

    let signExtend (w8 : uint8) : uint16 = 
        if w8 < 0x80uy then 
            (uint16)w8 
        else 
            (uint16)w8 ||| 0xFF00us

    // YoLo
    let toStr x = x.ToString()

namespace global

module Strings =
    open Microsoft.FSharp.Reflection

    let toUnionCase<'a> s =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None


// YoLo
// Path.combine
// GetExecutingAssembly
// Prelude