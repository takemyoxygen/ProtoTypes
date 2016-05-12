namespace ProtoTypes.Generation

open System
open FSharp.Quotations

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

/// Contains an implementation of deserialization methods for types generated from ProtoBuf messages
[<RequireQualifiedAccess>]
module Deserialization =

    /// Creates quotation that converts RawField quotation to target property type
    let private deserializeField (property: ProtoPropertyInfo) (rawField: Expr) =
        match property.UnderlyingType with
        | t when t = typeof<int> -> <@@ Codec.readInt32 %%rawField @@>
        | t when t = typeof<string> -> <@@ Codec.readString %%rawField  @@>
        | t when t = typeof<bool> -> <@@ Codec.readBool %%rawField  @@>
        | t when t = typeof<float> -> <@@ Codec.readDouble %%rawField  @@>
        | :? ProvidedTypeDefinition as ty-> 
            Expr.callStaticGeneric [ty] [rawField ] <@@ Codec.readEmbedded<Dummy> x @@> 
        | x -> notsupportedf "Deserialization of field %s of type %s is not supported yet" property.ProtoField.Name x.Name 

    let readFrom (ty: ProvidedTypeDefinition) (properties: ProtoPropertyInfo list) this buffer =
    
        // 1. Declare ResizeArray for all repeated fields
        // 2. Read all fields from given buffer
        // 3. Iterate over fields read: if FieldNum matches field position - 
        //    set corresponding property or add value to the ResizeArray (for repeated fields)
        // 4. Convert ResizeArray to lists and set repeated fields
        // 5. Return buffer
    
        try
        
        // for repeated rules - map from property to variable
        let resizeArrays =
            properties
            |> Seq.filter (fun prop -> prop.ProtoField.Rule = Repeated)
            |> Seq.map (fun prop -> prop, Var(prop.ProvidedProperty.Name, Expr.makeGenericType [prop.UnderlyingType] typedefof<ResizeArray<_>>))
            |> dict

        let samePosition field idx = <@@ (%%field: RawField).FieldNum = idx @@>

        /// For required and optional fields - set the property directly;
        /// for repeated - add to corresponding ResizeArray
        let handleField (property: ProtoPropertyInfo) (field: Expr) =
            let value = deserializeField property field
            
            match property.ProtoField.Rule with
            | Repeated -> 
                let list = Expr.Var(resizeArrays.[property])
                match property.TypeKind with
                | Class ->
                    Expr.callStaticGeneric 
                        [list.Type.GenericTypeArguments.[0]]
                        [Expr.Coerce(list, typeof<obj>); value]
                        <@@ ResizeArray.add x x @@>
                | _ ->
                    let addMethod = list.Type.GetMethod("Add")
                    Expr.Call(list, addMethod, [value])
            | Optional ->
                let someValue = Expr.callStaticGeneric [value.Type] [value] <@@ Option.some x @@> 
                Expr.PropertySet(this, property.ProvidedProperty, someValue)
            | Required ->
                Expr.PropertySet(this, property.ProvidedProperty, value)

        /// Converts ResizeArray to immutable list and sets corresponding repeated property
        let setRepeatedProperty property (resizeArrayVar: Var) =
            let itemTy = resizeArrayVar.Type.GenericTypeArguments.[0]
            let list = 
                match property.TypeKind with
                | Class ->
                    Expr.callStaticGeneric 
                        [itemTy] 
                        [Expr.Coerce(Expr.Var(resizeArrayVar), typeof<obj>)]
                        <@@ ResizeArray.toList x @@> 
                | _ -> 
                    Expr.callStaticGeneric [itemTy] [Expr.Var(resizeArrayVar)] <@@ List.ofSeq<_> x @@>

            Expr.PropertySet(this, property.ProvidedProperty, list)

        let fieldLoop = Expr.forLoop <@@ Codec.decodeFields %%buffer @@> (fun field ->
            properties
            |> Seq.fold
                (fun acc prop ->
                    Expr.IfThenElse(
                        samePosition field prop.ProtoField.Position,
                        handleField prop field,
                        acc))
                (Expr.Value(())))

        let setRepeatedFields =
            resizeArrays
            |> Seq.map (fun pair -> setRepeatedProperty pair.Key pair.Value)
            |> List.ofSeq

        let create ty = Expr.callStaticGeneric [ty] [] <@@ create<_>() @@>
        
        let body = fieldLoop :: setRepeatedFields @ [buffer] |> Expr.sequence

        resizeArrays.Values
        |> Seq.fold (fun acc var -> Expr.Let(var, create var.Type, acc)) body

        with
        | ex ->
           printfn "Failed to generate Deserialize method for type %s. Details: %O" ty.Name ex
           reraise()