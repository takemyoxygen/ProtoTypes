namespace ProtoTypes

open System
open FSharp.Quotations

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

module Deserialization =

    let readFields (zcb: ZeroCopyBuffer) = seq {
        while (not zcb.IsEof) && zcb.Array.[int zcb.Position] > 7uy do
            yield WireFormat.decodeField zcb
    }
    
    let deserializeFieldValue<'T> f field = 
        let result = ref Unchecked.defaultof<'T>
        f result field
        !result
        
    let deserialize<'T when 'T :> Message and 'T : (new: unit -> 'T)> buffer =
        let x = new 'T()
        x.ReadFrom buffer |> ignore
        x

    // TODO move these methods and similar methods from Serialization to separate
    // module, something line Codec.encodeInt32/Codec.decodeInt32 etc.
    let deserializeString = deserializeFieldValue Serializer.hydrateString
    let deserializeInt = deserializeFieldValue Serializer.hydrateInt32
    let deserializeBool = deserializeFieldValue Serializer.hydrateBool
    let deserializeDouble = deserializeFieldValue Serializer.hydrateDouble
    let deserializeEmbedded<'T when 'T :> Message and 'T : (new: unit -> 'T)> (field: RawField) = 
        match field with
        | LengthDelimited(_, segment) ->
            let buffer = ZeroCopyBuffer segment
            deserialize<'T> buffer
        | _ -> failwith "Invalid format of the field: %O" field

    let deserializeField (property: ProtoPropertyInfo) (rawField: Expr) =
        let targetTy =
            match property.ProtoField.Rule with
            | Repeated | Optional -> 
                property.ProvidedProperty.PropertyType.GetGenericArguments().[0]
            | _ -> property.ProvidedProperty.PropertyType
            
        let intType = typeof<int>
            
        
        match targetTy with
        | t when t = typeof<int> -> <@@ deserializeInt %%rawField @@>
        | t when t = typeof<string> -> <@@ deserializeString %%rawField  @@>
        | t when t = typeof<bool> -> <@@ deserializeBool %%rawField  @@>
        | t when t = typeof<float> -> <@@ deserializeDouble %%rawField  @@>
        | :? ProvidedTypeDefinition -> 
            let deserializeMethod = 
                <@@ deserializeEmbedded<Dummy> x @@> 
                |> Expr.getMethodDef 
                |> Expr.makeGenericMethod [targetTy]
            Expr.Call(deserializeMethod, [rawField])
        | _ -> notsupportedf "Deserialization of field %s of type %s is not supported" property.ProtoField.Name targetTy.Name 

        // let def =
        //     <@@ Unchecked.defaultof<_> @@>
        //     |> Expr.getMethodDef
        //     |> Expr.makeGenericMethod [targetTy]

    let addToList<'T> (list: obj) (item: 'T) =
        let list = list :?> ResizeArray<'T>
        list.Add item

    let toList<'T> (mutableList: obj) = mutableList :?> ResizeArray<'T> |> List.ofSeq

    let create<'T when 'T: (new: unit -> 'T)>() = new 'T()
    
    let some x = Some x

    let readFrom (ty: ProvidedTypeDefinition) (properties: ProtoPropertyInfo list) this buffer =
        try
        
        // for repeated rules - map from property to variable
        let listVars =
            properties
            |> Seq.filter (fun prop -> prop.ProtoField.Rule = Repeated)
            |> Seq.map (fun prop -> prop, Var(prop.ProvidedProperty.Name, Expr.makeGenericType [prop.ProvidedProperty.PropertyType.GenericTypeArguments.[0]] typedefof<ResizeArray<_>>))
            |> dict

        let eq field idx = <@@ (%%field: RawField).FieldNum = idx @@>

        let set (property: ProtoPropertyInfo) (field: Expr) =
            let value = deserializeField property field
            if property.ProtoField.Rule = Repeated then
                let list = Expr.Var(listVars.[property])
                let addMethod =
                    <@@ addToList x x @@>
                    |> Expr.getMethodDef
                    |> Expr.makeGenericMethod [list.Type.GenericTypeArguments.[0]]

                Expr.Call(addMethod, [Expr.Coerce(list, typeof<obj>); value])
            elif property.ProtoField.Rule = Optional then
                let some = <@@ some x @@> |> Expr.getMethodDef |> Expr.makeGenericMethod [value.Type]
                Expr.PropertySet(this, property.ProvidedProperty, Expr.Call(some, [value]))
            else
                Expr.PropertySet(this, property.ProvidedProperty, value)

        let setRepeated property (var: Var) =
            let itemTy = var.Type.GenericTypeArguments.[0]
            let toListMethod =
                <@@ toList x @@>
                |> Expr.getMethodDef
                |> Expr.makeGenericMethod [itemTy]

            let list = Expr.Call(toListMethod, [Expr.Coerce(Expr.Var(var), typeof<obj>)])
            Expr.PropertySet(this, property.ProvidedProperty, list)

        let fieldLoop = Expr.iterate <@@readFields %%buffer @@> (fun field ->
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
        let m = 
            <@@ deserialize<Dummy> x @@>
            |> Expr.getMethodDef
            |> Expr.makeGenericMethod [ty]

        Expr.Call(m, [buffer])
//        let msgVar = Var("msg", ty)
//        let msgExpr = Expr.Var msgVar
//        let create = <@@ create<_> () @@> |> Expr.getMethodDef |> Expr.makeGenericMethod [ty]
//        let readFrom = ty.GetMethod("ReadFrom")
//        let created = Expr.Call(create, [])
//        let read = Expr.Call(msgExpr, readFrom, [buffer])
//        Expr.Let(
//            msgVar, 
//            created, 
//            Expr.Sequential(
//                read,
//                msgExpr)) 
        // try
        
        // // for repeated rules - map from property to variable
        // let listVars =
        //     properties
        //     |> Seq.filter (fun prop -> prop.ProtoField.Rule = Repeated)
        //     |> Seq.map (fun prop -> prop, Var(prop.ProvidedProperty.Name, Expr.makeGenericType [prop.ProvidedProperty.PropertyType.GenericTypeArguments.[0]] typedefof<ResizeArray<_>>))
        //     |> dict

        // let msgVar = Var("msg", ty)
        // let msgExpr = Expr.Var msgVar

        // let eq field idx = <@@ (%%field: RawField).FieldNum = idx @@>

        // let set (property: ProtoPropertyInfo) (field: Expr) =
        //     if property.ProtoField.Rule = Repeated
        //     then
        //         let list = Expr.Var(listVars.[property])
        //         let addMethod =
        //             <@@ addToList x x @@>
        //             |> Expr.getMethodDef
        //             |> Expr.makeGenericMethod [list.Type.GenericTypeArguments.[0]]

        //         Expr.Call(addMethod, [Expr.Coerce(list, typeof<obj>); deserializeField property field])
        //     else
        //         Expr.PropertySet(msgExpr, property.ProvidedProperty, deserializeField property field)

        // let setRepeated property (var: Var) =
        //     let itemTy = var.Type.GenericTypeArguments.[0]
        //     let toListMethod =
        //         <@@ toList x @@>
        //         |> Expr.getMethodDef
        //         |> Expr.makeGenericMethod [itemTy]

        //     let list = Expr.Call(toListMethod, [Expr.Coerce(Expr.Var(var), typeof<obj>)])
        //     Expr.PropertySet(msgExpr, property.ProvidedProperty, list)

        // let fieldLoop = Expr.iterate <@@readFields %%buffer @@> (fun field ->
        //     properties
        //     |> Seq.fold
        //         (fun acc prop ->
        //             Expr.IfThenElse(
        //                 eq field prop.ProtoField.Position,
        //                 set prop field,
        //                 acc))
        //         (Expr.Value(())))

        // let setRepeatedFields =
        //     listVars
        //     |> Seq.map (fun pair -> setRepeated pair.Key pair.Value)
        //     |> List.ofSeq

        // let create ty =
        //     let createMethod = <@@ create<_>() @@> |> Expr.getMethodDef |> Expr.makeGenericMethod [ty]
        //     Expr.Call(createMethod, [])

        // let listsDefinitions =
        //     listVars.Values
        //     |> Seq.fold
        //         (fun acc var -> Expr.Let(var, create var.Type, acc))
        //         (fieldLoop :: setRepeatedFields @ [msgExpr] |> Expr.sequence)

        // Expr.Let(msgVar, Expr.create ty, listsDefinitions)

        // with
        // | ex ->
        //    printfn "Failed to generate Deserialize method for type %s. Details: %O" ty.Name ex
        //    reraise()

        // Generated expression should look like:
        //<@@

        //    let msg = new MessageToDeserialize()

        //    let repeatedInt_list = ResizeArray<int>()
        //    let repeatedString_list = ResizeArray<string>()

        //    for field in readFields %%buffer do
        //        if field.FieldNum = 1
        //        then msg.StringField <- deserializeString field
        //        elif field.FieldNum = 2
        //        then repeatedInt_list.Add(deserializeInt32 field)
        //        // and so on

        //    msg.RepeatedIntField <- List.ofSeq repeatedInt_list
        //    msg.RepeatedStringField <- List.ofSeq repeatedString_list

        //    msg

        //@@>
