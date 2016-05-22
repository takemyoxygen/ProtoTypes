namespace ProtoTypes.Generation

open System
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Parser.Ast
open Froto.Core
open Froto.Core.Encoding

[<RequireQualifiedAccess>]
module internal TypeGen =
    
    let private applyRule rule (fieldType: Type) = 
        match rule with
        | Required -> fieldType
        | Optional -> Expr.makeGenericType [fieldType] typedefof<option<_>> 
        | Repeated -> Expr.makeGenericType [fieldType] typedefof<list<_>>

    let private createProperty scope (lookup: TypesLookup) (field: ProtoField) =

        let typeKind, propertyType = 
            match TypeResolver.resolve scope field.Type lookup with
            | Some(Enum, t) -> TypeKind.Enum, typeof<int>
            | Some(kind, t) -> kind, t
            | None -> invalidOp <| sprintf "Unable to resolve type '%s'" field.Type
        
        let propertyType = applyRule field.Rule propertyType
        let propertyName = Naming.snakeToPascal field.Name
        let property, backingField = Provided.readWriteProperty propertyType propertyName
        let propertyInfo = 
            { ProvidedProperty = property;
              TypeKind = typeKind;
              Position = field.Position;
              ProtobufType = field.Type;
              Rule = field.Rule }
            
        propertyInfo, backingField


    let private createSerializeMethod typeInfo =
        let serialize =
            ProvidedMethod(
                "Serialize",
                [ ProvidedParameter("buffer", typeof<ZeroCopyBuffer>) ],
                typeof<ZeroCopyBuffer>,
                InvokeCode = (fun args -> Serialization.serializeExpr typeInfo args.[1] args.[0]))

        serialize.SetMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.Public)

        serialize
        
    let private createReadFromMethod typeInfo = 
        let readFrom = 
            ProvidedMethod(
                "LoadFrom",
                [ProvidedParameter("buffer", typeof<ZeroCopyBuffer>)],
                typeof<System.Void>,
                InvokeCode = (fun args -> Deserialization.readFrom typeInfo args.[0] args.[1]))

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
            TypeResolver.resolveNonScalar scope enum.Name lookup
            |> Option.require (sprintf "Enum '%s' is not defined" enum.Name)
        
        enum.Items
        |> Seq.map (fun item -> Naming.upperSnakeToPascal item.Name, item.Value)
        |> Provided.addEnumValues providedEnum
        
        providedEnum

    let rec createType scope (lookup: TypesLookup) (message: ProtoMessage) = 
        let _, providedType = 
            TypeResolver.resolveNonScalar scope message.Name lookup 
            |> Option.require (sprintf "Type '%s' is not defined" message.Name)
        
        let nestedScope = scope +.+ message.Name
        message.Enums |> Seq.map (createEnum nestedScope lookup) |> Seq.iter providedType.AddMember
        message.Messages |> Seq.map (createType nestedScope lookup) |> Seq.iter providedType.AddMember

        let properties = message.Fields |> List.map (createProperty nestedScope lookup)
        let propertiesInfo = properties |> List.map fst

        providedType.AddMembers(properties |> List.map snd)
        providedType.AddMembers(propertiesInfo |> List.map (fun p -> p.ProvidedProperty))
        providedType.AddMember <| Provided.ctor()
        
        let oneOfGroups = 
            message.Parts 
            |> Seq.choose (fun x -> match x with | TOneOf(name, members) -> Some((name, members)) | _ -> None)
            |> Seq.map (fun (name, members) -> OneOf.generateOneOf nestedScope lookup name members)
            |> Seq.fold (fun all (info, members) -> providedType.AddMembers members; info::all) []
            
        let typeInfo = { Type = providedType; Properties = propertiesInfo; OneOfGroups = oneOfGroups }

        let serializeMethod = createSerializeMethod typeInfo
        providedType.AddMember serializeMethod
        providedType.DefineMethodOverride(serializeMethod, typeof<Message>.GetMethod("Serialize"))
        
        let readFromMethod = createReadFromMethod typeInfo
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