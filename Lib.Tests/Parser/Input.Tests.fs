module Lib.Parser.TextInput.Tests

open FsUnit.Xunit
open Xunit

let rec readAllChars input = 
    [ let input', copt = nextChar input
      match copt with
      | None -> ()
      | Some c -> 
          yield c
          yield! readAllChars input' ]

let ``nextChar tests data`` : obj array seq = 
    seq { 
        yield ("", [])
        yield ("a", [ 'a' ])
        yield ("ab", [ 'a'; 'b' ])
        yield ("a\nb", [ 'a'; '\n'; 'b'])
    }
    |> Seq.map (fun (a, b) -> 
           [| box a
              box b |])

[<Theory>]
[<MemberData("nextChar tests data")>]
let ``nextChar tests`` (i : string, o : char list) : unit = 
    let res = fromStr i |> readAllChars
    res |> List.map char |> should equal o
