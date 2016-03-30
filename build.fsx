#r "./packages/FAKE/tools/FakeLib.dll"

open Fake

let outputDirectory  = "./build/"
let projects  = !! "/**/*.fsproj"
let version = "0.1" 

Target "Clean" (fun _ ->
    CleanDir outputDirectory
)

Target "Build" (fun _ ->
    MSBuildDebug outputDirectory "Build" projects
    |> Log "AppBuild-Output: "
)

"Clean"
  ==> "Build"

RunTargetOrDefault "Build"
