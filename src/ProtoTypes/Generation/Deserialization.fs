namespace ProtoTypes.Generation

open System
open FSharp.Quotations

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

module Deserialization =

    /// Creates quotation that converts RawField quotation to target property type
    let deserializeField (property: ProtoPropertyInfo) (rawField: Expr) =
        match property.UnderlyingType with
        | t when t = typeof<int> -> <@@ Codec.readInt %%rawField @@>
        | t when t = typeof<string> -> <@@ Codec.readString %%rawField  @@>
        | t when t = typeof<bool> -> <@@ Codec.readBool %%rawField  @@>
        | t when t = typeof<float> -> <@@ Codec.readDouble %%rawField  @@>
        | :? ProvidedTypeDefinition as ty-> 
            Expr.callStaticGeneric [ty] [rawField ] <@@ Codec.readEmbedded<Dummy> x @@> 
        | x -> notsupportedf "Deserialization of field %s of type %s is not supported yet" property.ProtoField.Name x.Name 

    let readFrom (ty: ProvidedTypeDefinition) (properties: ProtoPropertyInfo list) this buffer =
        try
        
        // for repeated rules - map from property to variable
        let listVars =
            properties
            |> Seq.filter (fun prop -> prop.ProtoField.Rule = Repeated)
            |> Seq.map (fun prop -> prop, Var(prop.ProvidedProperty.Name, Expr.makeGenericType [prop.UnderlyingType] typedefof<ResizeArray<_>>))
            |> dict

        let eq field idx = <@@ (%%field: RawField).FieldNum = idx @@>

        let set (property: ProtoPropertyInfo) (field: Expr) =
            let value = deserializeField property field
            
            match property.ProtoField.Rule with
            | Repeated -> 
                let list = Expr.Var(listVars.[property])
                Expr.callStaticGeneric 
                    [list.Type.GenericTypeArguments.[0]]
                    [Expr.Coerce(list, typeof<obj>); value]
                    <@@ ResizeArray.add x x @@>
            | Optional ->
                let someValue = Expr.callStaticGeneric [value.Type] [value] <@@ Option.some x @@> 
                Expr.PropertySet(this, property.ProvidedProperty, someValue)
            | Required ->
                Expr.PropertySet(this, property.ProvidedProperty, value)

        let setRepeated property (var: Var) =
            let itemTy = var.Type.GenericTypeArguments.[0]

            let list = 
                Expr.callStaticGeneric 
                    [itemTy] 
                    [Expr.Coerce(Expr.Var(var), typeof<obj>)]
                    <@@ ResizeArray.toList x @@> 

            Expr.PropertySet(this, property.ProvidedProperty, list)

        let fieldLoop = Expr.forLoop <@@ Codec.decodeFields %%buffer @@> (fun field ->
            properties
            |> Seq.fold
                (fun acc prop ->
                    Expr.IfThenElse(
                        eq field prop.ProtoField.Position,
                        set prop field,
                        acc))
                (Expr.Value(())))

        let setRepeatedFields =
            listVars
            |> Seq.map (fun pair -> setRepeated pair.Key pair.Value)
            |> List.ofSeq

        let create ty = Expr.callStaticGeneric [ty] [] <@@ create<_>() @@>

        listVars.Values
        |> Seq.fold
            (fun acc var -> Expr.Let(var, create var.Type, acc))
            (fieldLoop :: setRepeatedFields @ [buffer] |> Expr.sequence)

        with
        | ex ->
           printfn "Failed to generate Deserialize method for type %s. Details: %O" ty.Name ex
           reraise()

    let deserializeExpr (ty: ProvidedTypeDefinition) buffer =
        Expr.callStaticGeneric [ty] [buffer] <@@ Codec.deserialize<Dummy> x @@>