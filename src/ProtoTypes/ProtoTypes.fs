namespace ProtoTypes

open System
open System.IO
open System.Reflection

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices

open Froto.Parser.Model

[<TypeProvider>]
type ProtocolBuffersTypeProviderCreator(config : TypeProviderConfig) as this= 
    inherit TypeProviderForNamespaces()
    
    let ns = typeof<ProtocolBuffersTypeProviderCreator>.Namespace
    let asm = Assembly.GetExecutingAssembly()
    
    let protobufProvider = ProvidedTypeDefinition(asm, ns, "ProtocolBuffersTypeProvider", Some typeof<obj>)
    
    let parameters = [ProvidedStaticParameter("pathToFile", typeof<string>)]
    
    do 
        protobufProvider.DefineStaticParameters(parameters, fun typeName args ->
            let provider = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, HideObjectMethods = true)
            let pathToFile = args.[0] :?> string

            let protoLocation = 
                if Path.IsPathRooted pathToFile then pathToFile
                else config.ResolutionFolder </> pathToFile

            let protoFile = ProtoFile.ParseFile protoLocation
            
            protoFile.Messages
            |> Seq.map TypeGen.typeForMessage
            |> Seq.iter provider.AddMember
            
            provider)
            
        this.AddNamespace(ns, [protobufProvider])

[<assembly:TypeProviderAssembly>] 
do()
