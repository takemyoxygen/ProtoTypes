namespace ProtoTypes.Generation

open System
open System.Reflection
open FSharp.Quotations

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

[<RequireQualifiedAccess>]
module internal Provided =

    let message name = ProvidedTypeDefinition(name, Some typeof<Message>, IsErased = false)

    let enum name = 
        let enumTy = ProvidedTypeDefinition(name, Some typeof<Enum>, IsErased = false)
        enumTy.SetEnumUnderlyingType typeof<int>
        enumTy
    
    let addEnumValues (enum: ProvidedTypeDefinition) =
        Seq.map(fun (name, value) ->  ProvidedLiteralField(name, enum, value))
        >> Seq.iter enum.AddMember

    let readWriteProperty propertyType name = 
        let field = ProvidedField(Naming.pascalToCamel name, propertyType)
        field.SetFieldAttributes(FieldAttributes.InitOnly ||| FieldAttributes.Private)
        
        let property = 
            ProvidedProperty(name, propertyType,
                GetterCode = (fun args -> Expr.FieldGet(args.[0], field)),
                SetterCode = (fun args -> Expr.FieldSet(args.[0], field, args.[1])))

        property, field

    /// Creates an empty parameterless constructor
    let ctor () = ProvidedConstructor([], InvokeCode = (fun _ -> Expr.Value(())))