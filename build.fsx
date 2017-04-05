#r "./packages/FAKE/tools/FakeLib.dll"
#r "./packages/FSharpLint/FSharpLint.Application.dll"
#r "./packages/FSharpLint/FSharpLint.FAKE.dll"

open Fake
open Fake.Testing
open FSharpLint.FAKE
open System
open System.IO

MSBuildDefaults <- { MSBuildDefaults with Verbosity = Some MSBuildVerbosity.Minimal }

// Directories
let buildDir  = __SOURCE_DIRECTORY__ @@ @"build"
let testDir  = __SOURCE_DIRECTORY__ @@ @"build"

// Filesets
let solutionFile = "Fs5150.sln"

let msbuildProps = [
    "Configuration", "Debug"
    "Platform", "Any CPU"
]

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir]

    !! solutionFile
    |> MSBuild buildDir "Clean" msbuildProps
    |> ignore
)

Target "Rebuild" DoNothing

Target "Lint" (fun _ ->
    !! "**/*.fsproj"
        |> Seq.iter (FSharpLint (fun p -> { p with FailBuildIfAnyWarnings = true })))

Target "Build" (fun _ ->
    !! solutionFile
    |> MSBuild buildDir "Build" msbuildProps
    |> ignore
)

let runTest pattern =
    fun _ ->
        !! (buildDir @@ pattern)
        |> xUnit (fun p ->
            { p with
                ToolPath = findToolInSubPath "xunit.console.exe" (currentDirectory @@ "tools" @@ "xUnit")
                WorkingDir = Some testDir })

Target "Test" DoNothing
Target "UnitTests" (runTest "*.Tests*.dll")

"Lint" ==> "Build"
"Clean" ?=> "Lint"
"Clean" ?=> "Build"
"Clean" ==> "Rebuild" 
"Build" ==> "Rebuild" 
"Build" ?=> "UnitTests" ==> "Test"
"Rebuild" ==> "Test"

// start build
RunTargetOrDefault "Test"