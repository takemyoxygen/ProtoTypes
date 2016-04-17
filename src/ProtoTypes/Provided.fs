namespace ProtoTypes

open System.Reflection

open Microsoft.FSharp.Quotations

open ProviderImplementation.ProvidedTypes

[<RequireQualifiedAccess>]
module internal Provided =
    
    let addEnumValues (enum: ProvidedTypeDefinition) =
        Seq.map(fun (name, value) ->  ProvidedLiteralField(name, typeof<int>, value))
        >> Seq.iter enum.AddMember
        
    let readOnlyProperty propertyType name =
        let field = ProvidedField(Naming.pascalToCamel name, propertyType)
        field.SetFieldAttributes(FieldAttributes.InitOnly ||| FieldAttributes.Private)

        let property = ProvidedProperty(name, propertyType)
        property.GetterCode <- (fun args -> Expr.FieldGet(args.[0], field))
        
        property, field