module Lib.Parser.Combinators.Tests

open Xunit
open FsCheck
open FsUnit.Xunit
open Lib.Parser.Core
open Lib.Parser.Tokens
open System

[<Fact>]
let ``mapP - from char to int``() = 
    let p1 = '1'
             |> pchar
             |>> (Char.GetNumericValue >> int)
    run p1 () "1BC"
    |> printResult
    |> should equal "1 [State: 1 1BC]"

[<Fact>]
let ``Functor Law - identity``() = 
    let law a s = 
        let x = run (a |>> id) () s
        let y = run a () s
        x = y
    Check.QuickThrowOnFailure law

[<Fact>]
let ``Functor Law - composition``() = 
    let law f g a s = run (mapP (f >> g) a) () s = run (((mapP f) >> (mapP g)) a) () s
    Check.QuickThrowOnFailure law

[<Fact>]
let ``Applicative Law - identity``() = 
    let law f s = run (returnM id <*> f) () s = run f () s
    Check.QuickThrowOnFailure law

[<Fact>]
let ``Applicative Law - homomorphism``() = 
    let law f x s = run (returnM f <*> returnM x) () s = run (returnM (f x)) () s
    Check.QuickThrowOnFailure law

[<Fact>]
let ``Applicative Law - interchange``() = 
    let law u y s = run (u <*> returnM y) () s = run (returnM ((|>) y) <*> u) () s
    Check.QuickThrowOnFailure law

[<Fact>]
let ``Applicative Law - composition``() = 
    let law u v w s = run (returnM (<<) <*> u <*> v <*> w) () s = run (u <*> (v <*> w)) () s
    Check.QuickThrowOnFailure law

let parseABC = pstring "ABC" <?> "ABC"

[<Fact>]
let ``sequence - list of parsers - good input``() = 
    let res = run parseABC () "ABCDE"
    res
    |> printResult
    |> should equal "\"ABC\" [State: 3 ABCDE]"

[<Theory>]
[<InlineData("A|CDE", "1: A|CDE: Error parsing ABC. Unexpected '|'")>]
[<InlineData("AB|DE", "2: AB|DE: Error parsing ABC. Unexpected '|'")>]
[<InlineData("ACBDE", "1: ACBDE: Error parsing ABC. Unexpected 'C'")>]
let ``sequence - list of parsers - bad input`` (i, m) = 
    let res = run parseABC () i
    res
    |> printResult
    |> should equal m

[<Theory>]
[<InlineData("ABC", 0, "")>]
[<InlineData(" \tABC", 2, " \t")>]
[<InlineData("\t \n  ABC", 5, "\t \n  ")>]
let ``many - extract whitespaces`` (i, c, w) = 
    let res = run pwhitespace () i
    res
    |> printResult
    |> should equal (sprintf "\"%s\" [State: %d %s]" w c i)

[<Theory>]
[<InlineData("100ABC", "100 [State: 3 100ABC]")>]
[<InlineData("991", "991 [State: 3 991]")>]
let ``many1 - extract integer successfully`` (i, r) = 
    let res = run pinteger () i
    res
    |> printResult
    |> should equal r

[<Theory>]
[<InlineData("ABC", "0: ABC: Error parsing integer. Unexpected 'A'")>]
let ``many1 - unable to extract integer`` (i, m) = 
    let res = run pinteger () i
    res
    |> printResult
    |> should equal m

[<Theory>]
[<InlineData("100ABC", "100 [State: 3 100ABC]")>]
[<InlineData("-991", "-991 [State: 4 -991]")>]
let ``opt - extract signed integer successfully`` (i, r) = 
    let res = run psinteger () i
    res
    |> printResult
    |> should equal r

[<Fact>]
let ``throwaway left``() = 
    let ab_cd = (pstring "AB") .>> pwhitespace .>>. (pstring "CD")
    let res = run ab_cd () "AB \t\nCD..."
    res
    |> printResult
    |> should equal "(\"AB\", \"CD\") [State: 7 AB \t\nCD...]"

[<Fact>]
let ``throwaway right``() = 
    let ab_cd = (pstring "AB") .>>. (pwhitespace >>. (pstring "CD"))
    let res = run ab_cd () "AB \t\nCD..."
    res
    |> printResult
    |> should equal "(\"AB\", \"CD\") [State: 7 AB \t\nCD...]"

[<Fact>]
let ``between tests``() = 
    let pquotes = pchar '"'
    let ab_cd = between pquotes psinteger pquotes
    let res = run ab_cd () "\"-114\"---"
    res
    |> printResult
    |> should equal "-114 [State: 6 \"-114\"---]"

let comma = pchar ','

let ``sepBy1 successful tests data`` : obj array seq = 
    seq { 
        yield ("1;", 1, [ '1' ])
        yield ("1,2,3+-", 5, [ '1'; '2'; '3' ])
    }
    |> Seq.map (fun (a, b, c) -> 
           [| box a
              box b
              box c |])

[<Theory>]
[<MemberData("sepBy1 successful tests data")>]
let ``sepBy1 successful tests`` (i, c, r) : unit = 
    let oneOrMoreDigitList = sepBy1 pdigit comma
    let res = run oneOrMoreDigitList () i
    res
    |> printResult
    |> should equal (sprintf "%A [State: %d %s]" r c i)

[<Fact>]
let ``sepBy1 failure tests``() = 
    let oneOrMoreDigitList = sepBy1 pdigit comma
    let res = run oneOrMoreDigitList () "Z;"
    res
    |> printResult
    |> should equal "0: Z;: Error parsing digit. Unexpected 'Z'"

let ``sepBy test data`` : obj array seq = 
    seq { 
        yield ("1;", 1, [ '1' ])
        yield ("1,2,3+-", 5, [ '1'; '2'; '3' ])
        yield ("+-", 0, [])
    }
    |> Seq.map (fun (a, b, c) -> 
           [| box a
              box b
              box c |])

[<Theory>]
[<MemberData("sepBy test data")>]
let ``sepBy tests`` (i, c, r) : unit = 
    let zeroOrMoreDigitList = sepBy pdigit comma
    let res = run zeroOrMoreDigitList () i
    res
    |> printResult
    |> should equal (sprintf "%A [State: %d %s]" r c i)

[<Fact>]
let ``Monad Law - Left identity - Wrap and unwrap round trip``() = 
    let law f a s = run (returnM a >>= f) () s = run (f a) () s
    Check.QuickThrowOnFailure law

[<Fact>]
let ``Monad Law - Right identity - Unwrap and wrap round trip``() = 
    let law f s = run (f >>= returnM) () s = run f () s
    Check.QuickThrowOnFailure law

[<Fact>]
let ``Monad Law - Associative - Unwrap should be associative``() = 
    let law m f g s = run ((m >>= f) >>= g) () s = run (m >>= ((fun x -> f x >>= g))) () s
    Check.QuickThrowOnFailure law
