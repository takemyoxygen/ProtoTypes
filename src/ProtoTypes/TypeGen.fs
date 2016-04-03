namespace ProtoTypes

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations

open ProviderImplementation.ProvidedTypes
open Froto.Parser.Model

type Container = Dictionary<string, obj>

[<RequireQualifiedAccess>]
module internal TypeGen =

    let private convertScalarType = function
        | "double" -> typeof<double>
        | "float" -> typeof<float32>
        | "int32" | "sint32" | "fixed32" | "sfixed32" -> typeof<int>
        | "int64" | "sint64" | "fixed64" | "sfixed64" -> typeof<int64>
        | "uint32" -> typeof<uint32>
        | "uint64" -> typeof<uint64>
        | "string" -> typeof<string>
        | "bool" -> typeof<bool>
        | "bytes" -> typeof<byte[]>
        | x -> notsupportedf "Field type %s" x

    let private propertyForField (field: ProtoField) =
        let fieldType = convertScalarType field.Type
            
        let propertyType = 
            match field.Rule with
            | Required -> fieldType
            | Optional -> typedefof<Option<_>>.MakeGenericType(fieldType)
            | Repeated -> typedefof<list<_>>.MakeGenericType(fieldType)
            
        let propertyName = Naming.snakeToPascal field.Name

        ProvidedProperty(
            propertyName,
            propertyType, 
            GetterCode = (fun args -> 
                Expr.Coerce(
                    <@@ (%%args.[0]: Container).[propertyName] @@>,
                    propertyType)))

    /// Creates a constructor which intialized given list of properties. 
    /// Stores values in the dictionary. Body of the constructor would like like:
    /// type Type(prop1, prop2, prop3...) =
    ///     let container = new Dictionary<string, obj>()
    ///     container.Add("Prop1", prop1 :> obj)
    ///     container.Add("Prop2", prop2 :> obj)
    ///     container.Add("Prop3", prop3 :> obj)
    ///     container
    let private createConstructor (properties: ProvidedProperty list) =
        let parameters =
            properties
            |> List.map (fun prop -> ProvidedParameter(Naming.pascalToCamel prop.Name, prop.PropertyType))

        let add dict name value =
            let boxed = Expr.Coerce(value, typeof<obj>)
            <@@ (%%dict: Container).Add(name, %%boxed) @@>

        let constructorBody args =
            let container = Var("container", typeof<Container>)
            let containerExp = Expr.Var container
            let namedArgs = args |> List.mapi (fun i var -> properties.[i].Name, var) 
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
