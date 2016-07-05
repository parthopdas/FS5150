// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"
#r @"packages/FSharpLint/FSharpLint.Application.dll"
#r @"packages/FSharpLint/FSharpLint.FAKE.dll"

open Fake
open Fake.Testing
open FSharpLint.FAKE

// Directories
let buildDir  = "./build/"
let testDir  = "./build/"
let deployDir = "./deploy/"


// Filesets
let appReferences  =
    !! "/**/*.csproj"
      ++ "/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Lint" (fun _ ->
    !! "**/*.fsproj"
        |> Seq.iter (FSharpLint (fun p -> { p with FailBuildIfAnyWarnings = true })))

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
    !! (buildDir + "/*.Tests.dll")
        |> xUnit (fun p ->
          { p with
              ToolPath =
                findToolInSubPath "xunit.console.exe" (currentDirectory @@ "tools" @@ "xUnit")
              WorkingDir = Some testDir })
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
        -- "*.zip"
        |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

Target "Rebuild" DoNothing

//"Lint" ==> "Build"
"Build" ==> "Rebuild"
"Clean" ==> "Rebuild"
"Clean" ?=> "Build"
"Clean" ?=> "Lint"
"Build" ==> "Test"

// start build
RunTargetOrDefault "Build"
