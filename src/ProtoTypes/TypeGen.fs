namespace ProtoTypes

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations

open ProviderImplementation.ProvidedTypes
open Froto.Parser.Model

type Container = Dictionary<string, obj>

[<RequireQualifiedAccess>]
module internal TypeGen =

    let private asScalarType = function
        | "double" -> Some typeof<double>
        | "float" -> Some typeof<float32>
        | "int32" | "sint32" | "fixed32" | "sfixed32" -> Some typeof<int>
        | "int64" | "sint64" | "fixed64" | "sfixed64" -> Some typeof<int64>
        | "uint32" -> Some typeof<uint32>
        | "uint64" -> Some typeof<uint64>
        | "string" -> Some typeof<string>
        | "bool" -> Some typeof<bool>
        | "bytes" -> Some typeof<byte[]>
        | x -> None
    
    let applyRule rule (fieldType: Type) = 
        match rule with
        | Required -> fieldType
        | Optional -> typedefof<Option<_>>.MakeGenericType(fieldType)
        | Repeated -> typedefof<list<_>>.MakeGenericType(fieldType)

    let private propertyForField (typesRegistry: Map<string, Type>) (field: ProtoField) =

        let propertyType = 
            asScalarType field.Type 
            |> Option.otherwise (fun _ -> typesRegistry |> Map.tryFind field.Type)
            |> Option.require (sprintf "Type '%s' is not supported" field.Type)
            |> applyRule field.Rule
            
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
                <@@ new Container() @@>, 
                body)

        ProvidedConstructor(parameters, InvokeCode = constructorBody)
        
    let private createEnum (enum: ProtoEnum) =
        let providedEnum = ProvidedTypeDefinition(enum.Name, Some typeof<Enum>)
        providedEnum.SetEnumUnderlyingType typeof<int>
        
        enum.Items
        |> Seq.map (fun item -> ProvidedLiteralField(Naming.upperSnakeToPascal item.Name, typeof<int>, item.Value))
        |> Seq.iter providedEnum.AddMember
        
        providedEnum

    let typeForMessage (message: ProtoMessage) = 
        let messageType = new ProvidedTypeDefinition(message.Name, Some typeof<Container>, HideObjectMethods = true)

        let enums = message.Enums |> List.map createEnum
            
        // For now, if field is of enum type, int32 property will be generated
        let typesRegistry = enums |> Seq.map (fun e -> e.Name, typeof<int>) |> Map.ofSeq
        
        enums |> Seq.iter messageType.AddMember

        let properties = message.Fields |> List.map (propertyForField typesRegistry)
        properties |> Seq.iter messageType.AddMember
        
        messageType.AddMember <| createConstructor properties
        

        messageType
