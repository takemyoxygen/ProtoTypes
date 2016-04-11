#r "./packages/FAKE/tools/FakeLib.dll"

open System

open Fake
open Fake.Testing

let outputDirectory  = __SOURCE_DIRECTORY__ @@ "build"
let solution  = !! "ProtoTypes.sln"
let version = "0.1" 

Target "Clean" (fun _ ->
    CleanDir outputDirectory
)

Target "Build" (fun _ ->
    MSBuildDebug outputDirectory "Build" solution
    |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
    !! "./build/*.Tests.dll"
    |> NUnit3 (fun p -> 
        { p with 
            OutputDir = outputDirectory @@ "TestResult.xml"; 
            WorkingDir = outputDirectory })
)

Target "Watch" (fun _ ->
    use watcher = 
        !! "src/**/*.fs"
        |> WatchChanges (fun changes ->
            tracefn "%A" changes
            Run "Test")
            
    Console.ReadLine() |> ignore)

"Clean"
==> "Build"
==> "Test"

RunTargetOrDefault "Build"
