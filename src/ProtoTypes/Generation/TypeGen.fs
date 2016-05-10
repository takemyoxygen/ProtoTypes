namespace ProtoTypes.Generation

open System
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

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
    
    let private applyRule rule (fieldType: Type) = 
        match rule with
        | Required -> fieldType
        | Optional -> Expr.makeGenericType [fieldType] typedefof<option<_>> 
        | Repeated -> Expr.makeGenericType [fieldType] typedefof<list<_>>

    let private createProperty scope (lookup: TypesLookup) (field: ProtoField) =

        let findInLookup() =
            match TypesRegistry.resolve scope field.Type lookup with
            | Some(Enum, t) -> Some(TypeKind.Enum, typeof<int>)
            | Some(Class, t) -> Some(Class, t :> Type)
            | _ -> None

        let typeKind, propertyType = 
            asScalarType field.Type 
            |> Option.map (fun t -> Primitive, t)
            |> Option.otherwise findInLookup
            |> Option.map (fun (kind, t) -> kind, applyRule field.Rule t)
            |> Option.require (sprintf "Type '%s' is not supported" field.Type)
            
        let propertyName = Naming.snakeToPascal field.Name
        
        let property, backingField = Provided.readWriteProperty propertyType propertyName
        
        { ProvidedProperty = property; BackingField = backingField; ProtoField = field; TypeKind = typeKind }


    let private createSerializeMethod properties =
        let serialize =
            ProvidedMethod(
                "Serialize",
                [ ProvidedParameter("buffer", typeof<ZeroCopyBuffer>) ],
                typeof<ZeroCopyBuffer>,
                InvokeCode = (fun args -> Serialization.serializeExpr properties args.[1] args.[0]))

        serialize.SetMethodAttrs(MethodAttributes.Virtual)

        serialize
        
    let private createReadFromMethod properties targetType = 
        let readFrom = 
            ProvidedMethod(
                "LoadFrom",
                [ProvidedParameter("buffer", typeof<ZeroCopyBuffer>)],
                typeof<ZeroCopyBuffer>,
                InvokeCode = (fun args -> Deserialization.readFrom targetType properties args.[0] args.[1]))

        readFrom.SetMethodAttrs(MethodAttributes.Virtual)

        readFrom
        
    let private createDeserializeMethod properties targetType =
        let deserializeMethod = 
            ProvidedMethod(
                "Deserialize", 
                [ProvidedParameter("buffer", typeof<ZeroCopyBuffer>)], 
                targetType,
                InvokeCode = 
                    (fun args -> Expr.callStaticGeneric [targetType] [args.[0]] <@@ Codec.deserialize<Dummy> x @@>))
                
        deserializeMethod.SetMethodAttrs(MethodAttributes.Static ||| MethodAttributes.Public)
        
        deserializeMethod
    
    let private createEnum scope lookup (enum: ProtoEnum) =
        let _, providedEnum = 
            TypesRegistry.resolve scope enum.Name lookup
            |> Option.require (sprintf "Enum '%s' is not defined" enum.Name)
        
        enum.Items
        |> Seq.map (fun item -> Naming.upperSnakeToPascal item.Name, item.Value)
        |> Provided.addEnumValues providedEnum
        
        providedEnum

    let rec createType scope (lookup: TypesLookup) (message: ProtoMessage) = 
        let _, providedType = 
            TypesRegistry.resolve scope message.Name lookup 
            |> Option.require (sprintf "Type '%s' is not defined" message.Name)
        
        let nestedScope = scope +.+ message.Name
        message.Enums |> Seq.map (createEnum nestedScope lookup) |> Seq.iter providedType.AddMember
        message.Messages |> Seq.map (createType nestedScope lookup) |> Seq.iter providedType.AddMember

        let properties = message.Fields |> List.map (createProperty nestedScope lookup)

        properties |> Seq.iter (fun p -> providedType.AddMember p.ProvidedProperty; providedType.AddMember p.BackingField)
        providedType.AddMember <| Provided.ctor()
        
        let serializeMethod = createSerializeMethod properties
        providedType.AddMember serializeMethod
        providedType.DefineMethodOverride(serializeMethod, typeof<Message>.GetMethod("Serialize"))
        
        let readFromMethod = createReadFromMethod properties providedType
        providedType.AddMember readFromMethod
        providedType.DefineMethodOverride(readFromMethod, typeof<Message>.GetMethod("ReadFrom"))
        
        providedType.AddMember <| createDeserializeMethod properties providedType
        
        providedType
    
    /// For the given package e.g. "foo.bar.baz.abc" creates a hierarchy of nested types Foo.Bar.Baz.Abc 
    /// and returns pair of the first and last types in the hirarchy, Foo and Abc in this case
    let createNamespaceContainer (package: string) =
    
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