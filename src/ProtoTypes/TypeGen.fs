namespace ProtoTypes

open System

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices

open ProviderImplementation.ProvidedTypes
open Froto.Parser.Model

[<RequireQualifiedAccess>]
module internal TypeGen =

    let private toCamelCase (s: string) =
        s.Split('_')
        |> Seq.map (fun s -> Char.ToUpper(s.[0]).ToString() + (if s.Length > 1 then s.Substring(1) else String.Empty) )
        |> String.concat String.Empty

    let private propertyForField (field: ProtoField) =
        let fieldType, value = 
            match field.Type with
            | "int32" -> typeof<int>, box 0
            | "string" -> typeof<string>, box "string"
            | _ -> notsupportedf "Field type %s" field.Type
            
        let propertyType, propertyValue = 
            match field.Rule with
            | Required -> fieldType, value
            | Optional -> typedefof<Option<_>>.MakeGenericType(fieldType), box <| Some value
            | Repeated -> typedefof<list<_>>.MakeGenericType(fieldType), box [value]
            
        ProvidedProperty(toCamelCase field.Name, propertyType, GetterCode = (fun _ -> <@@ value @@>))

    let typeForMessage (message: ProtoMessage) = 
        let messageType = new ProvidedTypeDefinition(message.Name, Some typeof<obj>)
        
        message.Fields
        |> Seq.map propertyForField
        |> Seq.iter messageType.AddMember
        
        messageType.AddMember <| ProvidedConstructor([], InvokeCode = (fun _ -> <@@ new obj() @@>))
        
        messageType
