namespace ProtoTypes

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Core.CompilerServices

open ProviderImplementation.ProvidedTypes
open Froto.Parser.Model

type Container = Dictionary<string, obj>

[<RequireQualifiedAccess>]
module internal TypeGen =

    let private withFirstChar f (input: string) =
        if String.IsNullOrEmpty input then input
        else
            let first = input.[0] |> f |> string
            if input.Length > 1 then first + input.[1..]
            else first        

    let private toPascalCase (s: string) =
        s.Split('_')
        |> Seq.map (withFirstChar Char.ToUpper)
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
            
        let propertyName = toPascalCase field.Name
        let key = withFirstChar Char.ToLower propertyName
            
        ProvidedProperty(
            propertyName,
            propertyType, 
            GetterCode = (fun args -> 
                Expr.Coerce(
                    <@@ (%%args.[0]: Container).[key] @@>,
                    propertyType)))

    /// Creates a constructor which intialized given list of properties. 
    /// Stores values in the dictionary. Body of the constructor would like like:
    /// type Type(prop1, prop2, prop3...) =
    ///     let container = new Dictionary<string, obj>()
    ///     container.Add("prop1", prop1 :> obj)
    ///     container.Add("prop2", prop2 :> obj)
    ///     container.Add("prop3", prop3 :> obj)
    ///     container
    let private createConstructor (properties: ProvidedProperty list) =
        let parameters =
            properties
            |> List.map (fun prop -> ProvidedParameter(withFirstChar Char.ToLower prop.Name, prop.PropertyType))
        
        let addMethod = typeof<Container>.GetMethod("Add")

        let add dict name value =
            Expr.Call(dict, addMethod, [Expr.Value(name); Expr.Coerce(value, typeof<obj>)])

        let constructorBody args =
            let container = Var("container", typeof<Container>)
            let containerExp = Expr.Var container
            let namedArgs = args |> List.mapi (fun i var -> parameters.[i].Name, var) 
            let body = 
                List.foldBack 
                    (fun (name, value) prev -> Expr.Sequential(add containerExp name value, prev)) 
                    namedArgs
                    containerExp
                
            Expr.Let(
                container, 
                Expr.NewObject(typeof<Container>.GetConstructor([||]), []), 
                body)

        ProvidedConstructor(parameters, InvokeCode = constructorBody)

    let typeForMessage (message: ProtoMessage) = 
        let messageType = new ProvidedTypeDefinition(message.Name, Some typeof<Container>)
        
        let properties = message.Fields |> List.map propertyForField
        properties |> Seq.iter messageType.AddMember
        
        messageType.AddMember <| createConstructor properties
        
        messageType
