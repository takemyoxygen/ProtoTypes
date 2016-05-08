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
            <@@ Codec.readEmbedded<Dummy> x @@> 
            |> Expr.getMethodDef 
            |> Expr.makeGenericMethod [ty]
            |> Expr.callStatic [rawField]
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
                <@@ ResizeArray.add x x @@>
                |> Expr.getMethodDef
                |> Expr.makeGenericMethod [list.Type.GenericTypeArguments.[0]]
                |> Expr.callStatic [Expr.Coerce(list, typeof<obj>); value]
            | Optional ->
                let someValue = 
                    <@@ Option.some x @@> 
                    |> Expr.getMethodDef 
                    |> Expr.makeGenericMethod [value.Type]
                    |> Expr.callStatic [value]
                Expr.PropertySet(this, property.ProvidedProperty, someValue)
            | Required ->
                Expr.PropertySet(this, property.ProvidedProperty, value)

        let setRepeated property (var: Var) =
            let itemTy = var.Type.GenericTypeArguments.[0]
            let toListMethod =
                <@@ ResizeArray.toList x @@>
                |> Expr.getMethodDef
                |> Expr.makeGenericMethod [itemTy]

            let list = Expr.Call(toListMethod, [Expr.Coerce(Expr.Var(var), typeof<obj>)])
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

        let create ty =
            let createMethod = <@@ create<_>() @@> |> Expr.getMethodDef |> Expr.makeGenericMethod [ty]
            Expr.Call(createMethod, [])

        listVars.Values
        |> Seq.fold
            (fun acc var -> Expr.Let(var, create var.Type, acc))
            (fieldLoop :: setRepeatedFields @ [buffer] |> Expr.sequence)

        with
        | ex ->
           printfn "Failed to generate Deserialize method for type %s. Details: %O" ty.Name ex
           reraise()

    let deserializeExpr (ty: ProvidedTypeDefinition) buffer =
        <@@ Codec.deserialize<Dummy> x @@>
        |> Expr.getMethodDef
        |> Expr.makeGenericMethod [ty]
        |> Expr.callStatic [buffer]