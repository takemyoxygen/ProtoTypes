namespace ProtoTypes

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices

open ProviderImplementation.ProvidedTypes
open Froto.Parser.Model

[<RequireQualifiedAccess>]
module internal TypeGen =

    let private propertyForField (field: ProtoField) =
        let fieldType = 
            match field.Type with
            | "int32" -> typeof<int>
            | "string" -> typeof<string>
            | _ -> notsupportedf "Field type %s" field.Type
            
        let propertyType = 
            match field.Rule with
            | Required -> fieldType
            | Optional -> typedefof<Option<_>>.MakeGenericType(fieldType)
            | Repeated -> typedefof<list<_>>.MakeGenericType(fieldType)
            
        ProvidedProperty(field.Name, propertyType)

    let typeForMessage (message: ProtoMessage) = 
        let messageType = new ProvidedTypeDefinition(message.Name, Some typeof<obj>)
        
        message.Fields
        |> Seq.map propertyForField
        |> Seq.iter messageType.AddMember
        
        messageType.AddMember <| ProvidedConstructor([], InvokeCode = (fun _ -> <@@ new obj() @@>))
        
        messageType
