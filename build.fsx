#r "paket: groupref netcorebuild //"
#load ".fake/build.fsx/intellisense.fsx"
#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif

#nowarn "52"

open System
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Tools.Git
open Fake.JavaScript

let deployDir = "./deploy" |> Path.getFullName
let staticDir = "./static/assets" |> Path.getFullName
let dockerUser = "evelina"
let dockerOrg = "turinginst"
let dockerImageName = "twitcher"

let localConfig = staticDir + "/api-config.yaml"

Target.create "Clean" (fun _ ->
    !! "src/bin"
    ++ "src/obj"
    ++ "output"
    |> Seq.iter Shell.cleanDir
)

Target.create "DotnetRestore" (fun _ ->
    DotNet.restore
        (DotNet.Options.withWorkingDirectory __SOURCE_DIRECTORY__)
        "twitcher.sln"
)

Target.create "YarnInstall" (fun _ ->
    Yarn.install id
)

Target.create "Build" (fun _ ->

    // download API config yaml file
    let configFile = "https://raw.githubusercontent.com/alan-turing-institute/dodo/master/config.yml"
    let wc = new System.Net.WebClient()
    wc.DownloadFile(configFile, localConfig)

    Yarn.exec "webpack" id
)

Target.create "Watch" (fun _ ->
    Yarn.exec "webpack-dev-server" id
)

Target.create "DockerBuild" (fun _ ->

    // let result =
    //     DotNet.exec
    //         (DotNet.Options.withWorkingDirectory __SOURCE_DIRECTORY__)
    //         "fable"
    //         "webpack --port free -- -p"
            
    Fake.IO.Shell.copyRecursive "output" deployDir true |> ignore

    // if not result.OK then failwithf "dotnet fable failed with code %i" result.ExitCode
)

// Build order
"Clean"
    ==> "DotnetRestore"
    ==> "YarnInstall"
    ==> "Build"
    ==> "DockerBuild"

"Watch"
    <== [ "YarnInstall" ]

// start build
Target.runOrDefault "DockerBuild"
