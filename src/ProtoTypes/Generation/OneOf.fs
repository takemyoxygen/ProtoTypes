namespace ProtoTypes.Generation

open System.Reflection
open FSharp.Quotations

open Froto.Parser.Ast

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

[<RequireQualifiedAccess>]
module OneOf = 
    
    /// Generates members that represent "oneof" group. Similar to whan current version of C# code generator is doing
    /// https://developers.google.com/protocol-buffers/docs/reference/csharp-generated#oneof
    let generateOneOf scope (typesLookup: TypesLookup) (name: string) (members: POneOfStatement list) = 

        let oneofCaseEnum = Provided.enum <| Naming.snakeToPascal name + "OneofCase"
        
        let numberedMembers = members |> List.mapi (fun i m -> i + 1, m)
        ("None", 0) :: (numberedMembers |> List.map (fun (i, TOneOfField(name, _, _, _)) -> Naming.snakeToPascal name, i))
        |> Provided.addEnumValues oneofCaseEnum
        
        let caseField = ProvidedField(Naming.snakeToCamel name + "Case", typeof<int>)
        let caseProperty = ProvidedProperty(Naming.snakeToPascal name + "Case", typeof<int>)
        caseProperty.GetterCode <- fun args -> Expr.FieldGet(args.[0], caseField)

        numberedMembers
        |> List.collect (fun (i, TOneOfField(name, ptype, _, _)) -> 
            let _, fieldType =
                TypeResolver.resolvePType scope ptype typesLookup
                |> Option.map (fun (kind, ty) -> kind, Expr.makeGenericType [ty] typedefof<option<_>>)
                |> Option.require (sprintf "Unable to find type %A" ptype) 
                
            let field = ProvidedField(Naming.snakeToCamel name, fieldType)
            let property = ProvidedProperty(Naming.snakeToPascal name, fieldType)
            property.GetterCode <- fun args -> Expr.FieldGet(args.[0], field)
            property.SetterCode <- fun args -> 
                Expr.Sequential(
                    Expr.FieldSet(args.[0], field, args.[1]),
                    Expr.FieldSet(args.[0], caseField, Expr.Value(i)))
            [field :> MemberInfo; property :> MemberInfo])
        |> List.append [ oneofCaseEnum :> MemberInfo; caseField :> MemberInfo; caseProperty :> MemberInfo]