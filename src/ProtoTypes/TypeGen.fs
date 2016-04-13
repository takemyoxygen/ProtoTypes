namespace ProtoTypes

open System
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

type Underlying = Dictionary<string, obj>

type ReadOnlyPropertyMetadata = 
    { ProvidedProperty: ProvidedProperty;
      BackingField: ProvidedField;
      ProtoField: ProtoField; }
      
 [<RequireQualifiedAccess>]
module Provided =
    
    let addEnumValues (enum: ProvidedTypeDefinition) =
        Seq.map(fun (name, value) ->  ProvidedLiteralField(name, typeof<int>, value))
        >> Seq.iter enum.AddMember
        
    let readOnlyProperty propertyType name =
        let field = ProvidedField(Naming.pascalToCamel name, propertyType)
        field.SetFieldAttributes(FieldAttributes.InitOnly ||| FieldAttributes.Private)

        let property = ProvidedProperty(name, propertyType)
        property.GetterCode <- (fun args -> Expr.FieldGet(args.[0], field))
        
        property, field
      
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
        
        let property, backingField = Provided.readOnlyProperty propertyType propertyName
        
        { ProvidedProperty = property; BackingField = backingField; ProtoField = field }

    /// Creates a constructor which intializes backing fields for the given list of properties 
    let private createConstructor (properties: ReadOnlyPropertyMetadata list) =
        let parameters =
            properties
            |> List.map (fun prop -> ProvidedParameter(prop.BackingField.Name, prop.ProvidedProperty.PropertyType))

        let fieldsMap = 
            properties
            |> Seq.map (fun prop -> prop.BackingField.Name, prop.BackingField)
            |> Map.ofSeq
            
        let setField this arg = 
            match arg with
            | Var(v) ->
                match fieldsMap |> Map.tryFind v.Name with
                | Some(field) ->  Expr.FieldSet(this, field, arg)
                | None -> failwithf "Unable to find field '%s'" v.Name
            | _ -> Prelude.notsupportedf "Given expression is not supported: %A" arg

        let add dict name value =
            let boxed = Expr.Coerce(value, typeof<obj>)
            <@@ (%%dict: Underlying).Add(name, %%boxed) @@>

        let constructorBody args =
            let this::args = args
            match args |> List.map (setField this) with
            | [] -> Expr.Value(())
            | h::t -> t |> List.fold (fun acc setter -> Expr.Sequential(acc, setter)) h
                
        ProvidedConstructor(parameters, InvokeCode = constructorBody)
        
    let private createEnum scope lookup (enum: ProtoEnum) =
        let _, providedEnum = 
            TypesRegistry.resolve scope enum.Name lookup
            |> Option.require (sprintf "Enum '%s' is not defined" enum.Name)
        
        enum.Items
        |> Seq.map (fun item -> Naming.upperSnakeToPascal item.Name, item.Value)
        |> Provided.addEnumValues providedEnum
        
        providedEnum

    let rec typeForMessage scope (lookup: TypesLookup) (message: ProtoMessage) = 
        let _, providedType = 
            TypesRegistry.resolve scope message.Name lookup 
            |> Option.require (sprintf "Type '%s' is not defined" message.Name)
        
        let nestedScope = scope +.+ message.Name
        message.Enums |> Seq.map (createEnum nestedScope lookup) |> Seq.iter providedType.AddMember
        message.Messages |> Seq.map (typeForMessage nestedScope lookup) |> Seq.iter providedType.AddMember

        let properties = 
            message.Fields 
            |> List.map (propertyForField nestedScope lookup)

        properties |> Seq.iter (fun p -> providedType.AddMember p.ProvidedProperty; providedType.AddMember p.BackingField)
        
        providedType.AddMember <| createConstructor properties

        providedType
    
    /// For the given package e.g. "foo.bar.baz.abc" creates a hierarchy of nested types Foo.Bar.Baz.Abc 
    /// and returns pair of the first and last types in the hirarchy, Foo and Abc in this case
    let generatePackageContainer (package: string) =
    
        let rec loop names (current: ProvidedTypeDefinition) =
            match names with
            | [] -> current
            | x::xs -> 
                let nested = ProvidedTypeDefinition(Naming.snakeToPascal x, Some typeof<obj>, IsErased = false)
                current.AddMember nested
                loop xs nested
                
        let rootName::rest = package.Split('.') |> List.ofSeq
        let root = ProvidedTypeDefinition(Naming.snakeToPascal rootName, Some typeof<obj>, IsErased = false)
        let deepest = loop rest root
        root, deepest