namespace ProtoTypes

open System
open System.IO
open System.Reflection

open ProtoTypes.Core
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices

open Froto.Parser.Model

[<TypeProvider>]
type ProtocolBuffersTypeProviderCreator(config : TypeProviderConfig) as this= 
    inherit TypeProviderForNamespaces()
    
    let ns = typeof<ProtocolBuffersTypeProviderCreator>.Namespace
    let asm = Assembly.LoadFrom config.RuntimeAssembly
    let tempAssembly = Path.ChangeExtension(Path.GetTempFileName(), ".dll") |> ProvidedAssembly
    
    let protobufProvider = ProvidedTypeDefinition(asm, ns, "ProtocolBuffersTypeProvider", Some typeof<obj>, IsErased = false, HideObjectMethods = true)

    let parameters = [ProvidedStaticParameter("pathToFile", typeof<string>)]
    
    do 
        printfn "Starting creating a type provider..."
        protobufProvider.DefineStaticParameters(parameters, fun typeName args ->
            let provider = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, HideObjectMethods = true, IsErased = false)
            let tempAssembly = Path.ChangeExtension(Path.GetTempFileName(), ".dll") |> ProvidedAssembly
            
            let pathToFile = args.[0] :?> string

            let protoLocation = 
                if Path.IsPathRooted pathToFile then pathToFile
                else config.ResolutionFolder </> pathToFile
            
            printfn "Parsing proto file '%s'" protoLocation
            let protoFile = ProtoFile.ParseFile protoLocation
            
            let rootScope = protoFile.Packages |> Seq.tryHead |> Option.getOrElse String.Empty
            
            let container = 
                if String.IsNullOrEmpty rootScope
                then provider 
                else
                    let root, deepest = TypeGen.createNamespaceContainer rootScope
                    provider.AddMember root
                    deepest
            
            printfn "Discovering message types in proto files."
            let lookup = TypesRegistry.discoverTypes rootScope protoFile.Messages
            
            printfn "Generating members of discovered types."
            protoFile.Messages
            |> Seq.map (TypeGen.createType rootScope lookup)
            |> Seq.iter container.AddMember
            
            if config.IsHostedExecution then
                Testing.FormatProvidedType(container, true)
                |> printfn "%s"
            
            tempAssembly.AddTypes [provider]
            
            printfn "Provided is created. Returning"
            provider)
        
        printfn "Registering provider in the temp assembly"
        tempAssembly.AddTypes [protobufProvider]
        this.AddNamespace(ns, [protobufProvider])
        printfn "Type provider construction completed"

[<assembly:TypeProviderAssembly>] 
do()
