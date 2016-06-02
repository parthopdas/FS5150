namespace Lib

module Common =
    open System.Diagnostics

    let inline dprintfn fmt = Printf.ksprintf Debug.WriteLine fmt
    
    let inline tee fn x = x |> fn |> ignore; x

    let signExtend (w8 : uint8) : uint16 = 
        if w8 < 0x80uy then 
            (uint16)w8 
        else 
            (uint16)w8 ||| 0xFF00us

    let toStr x = x.ToString()
