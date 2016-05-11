#r "./packages/FAKE/tools/FakeLib.dll"

open System
open System.Net

open Fake
open Fake.AppVeyor
open Fake.Testing

let outputDirectory  = __SOURCE_DIRECTORY__ @@ "build"
let solution  = !! "ProtoTypes.sln"
let version = "0.1"
let testResultsXml = outputDirectory @@ "TestResult.xml"

Target "Clean" (fun _ ->
    CleanDir outputDirectory
)

Target "Build" (fun _ ->
    MSBuildDebug outputDirectory "Build" solution
    |> Log "AppBuild-Output: "
)

Target "RunTests" (fun _ ->
    !! "./build/*.Tests.dll"
    |> NUnit3 (fun p -> 
        { p with 
            OutputDir = testResultsXml; 
            WorkingDir = outputDirectory })
)

Target "UploadTestResults" (fun _ ->
    UploadTestResultsFile TestResultsType.NUnit3 testResultsXml
)

Target "Watch" (fun _ ->
    use watcher = 
        !! "src/**/*.fs"
        |> WatchChanges (fun changes ->
            tracefn "%A" changes
            Run "Test")
            
    Console.ReadLine() |> ignore)
    
Target "Test" DoNothing

"Clean"
==> "Build"
==> "RunTests"
==> "UploadTestResults"
==> "Test"

RunTargetOrDefault "Build"
