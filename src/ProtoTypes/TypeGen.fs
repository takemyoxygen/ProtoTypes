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

    let private propertyForField scope (lookup: TypesLookup) (field: ProtoField) =

        let findInLookup() =
            match TypesRegistry.resolve scope field.Type lookup with
            | Some(Enum, t) -> Some typeof<int>
            | Some(Class, t) -> Some(t :> Type)
            | None -> None

        let propertyType = 
            asScalarType field.Type 
            |> Option.otherwise findInLookup
            |> Option.require (sprintf "Type '%s' is not supported" field.Type)
            |> applyRule field.Rule
            
        let propertyName = Naming.snakeToPascal field.Name

        ProvidedProperty(
            propertyName,
            propertyType, 
            GetterCode = (fun args -> 
                Expr.Coerce(
                    <@@ ((%%args.[0]: obj) :?> Container).[propertyName] @@>,
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
                    (Expr.Coerce(containerExp, typeof<obj>))
                
            Expr.Let(
                container, 
                <@@ new Container() @@>, 
                body)

        ProvidedConstructor(parameters, InvokeCode = constructorBody)
        
    let private createEnum scope lookup (enum: ProtoEnum) =
        let _, providedEnum = 
            TypesRegistry.resolve scope enum.Name lookup
            |> Option.require (sprintf "Enum '%s' is not defined" enum.Name)
        
        enum.Items
        |> Seq.map (fun item -> ProvidedLiteralField(Naming.upperSnakeToPascal item.Name, typeof<int>, item.Value))
        |> Seq.iter providedEnum.AddMember
        
        providedEnum

    let rec typeForMessage scope (lookup: TypesLookup) (message: ProtoMessage) = 
        let _, providedType = 
            TypesRegistry.resolve scope message.Name lookup 
            |> Option.require (sprintf "Type '%s' is not defined" message.Name)
        
        let nestedScope = scope +.+ message.Name
        message.Enums |> Seq.map (createEnum nestedScope lookup) |> Seq.iter providedType.AddMember
        message.Messages |> Seq.map (typeForMessage nestedScope lookup) |> Seq.iter providedType.AddMember

        let properties = message.Fields |> List.map (propertyForField nestedScope lookup)
        properties |> Seq.iter providedType.AddMember
        
        providedType.AddMember <| createConstructor properties

        providedType
    
    /// For the given package e.g. "foo.bar.baz.abc" creates a hierarchy of nested types Foo.Bar.Baz.Abc 
    /// and returns pair of the first and last types in the hirarchy, Foo and Abc in this case
    let generatePackageContainer (package: string) =
    
        let rec loop names (current: ProvidedTypeDefinition) =
            match names with
            | [] -> current
            | x::xs -> 
                printfn "Creating type '%s' to put inside '%s'" x current.Name
                let nested = ProvidedTypeDefinition(Naming.snakeToPascal x, Some typeof<obj>)
                current.AddMember nested
                loop xs nested
                
        let rootName::rest = package.Split('.') |> List.ofSeq
        let root = ProvidedTypeDefinition(Naming.snakeToPascal rootName, Some typeof<obj>)
        let deepest = loop rest root
        root, deepest