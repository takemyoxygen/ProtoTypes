namespace ProtoTypes

open System
open FSharp.Quotations

open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

module Deserialization = 

    let readFields (zcb: ZeroCopyBuffer) = seq {
        while (not zcb.IsEof) && zcb.Array.[int zcb.Position] > 7uy do
            yield WireFormat.decodeField zcb
    }

    let deserializeString field =
        let result = ref String.Empty
        Serializer.hydrateString result field
        !result

    let deserializeField (property: ProtoPropertyInfo) (rawField: Expr) =
        let targetTy =
            if property.ProtoField.Rule = Repeated 
            then property.ProvidedProperty.PropertyType.GetGenericArguments().[0]
            else property.ProvidedProperty.PropertyType

        let def =
            <@@ Unchecked.defaultof<_> @@> 
            |> Expr.getMethodDef
            |> Expr.makeGenericMethod [targetTy]

        Expr.Call(def, [])

    let addToList<'T> (list: obj) (item: 'T) =
        let list = list :?> ResizeArray<'T>
        list.Add item

    let toList<'T> (mutableList: obj) = mutableList :?> ResizeArray<'T> |> List.ofSeq

    let createResizeArray<'T>() = ResizeArray<'T>()

    let create<'T when 'T: (new: unit -> 'T)>() = new 'T()

    let deserialize (ty: ProvidedTypeDefinition) (properties: ProtoPropertyInfo list) (buffer: Expr) = 
//        try
        // for repeated rules - map from property to variable
        let listVars = 
            properties
            |> Seq.filter (fun prop -> prop.ProtoField.Rule = Repeated)
            |> Seq.map (fun prop -> prop, Var(prop.ProvidedProperty.Name, Expr.makeGenericType [prop.ProvidedProperty.PropertyType.GenericTypeArguments.[0]] typedefof<ResizeArray<_>>))
            |> dict

        let msgVar = Var("msg", ty)
        let msgExpr = Expr.Var msgVar
        
        let eq field idx = <@@ (%%field: RawField).FieldNum = idx @@> 
        
        let set (property: ProtoPropertyInfo) (field: Expr) = 
            if property.ProtoField.Rule = Repeated
            then
                let list = Expr.Var(listVars.[property])
                let addMethod = 
                    <@@ addToList x x @@>
                    |> Expr.getMethodDef
                    |> Expr.makeGenericMethod [list.Type.GenericTypeArguments.[0]]

                Expr.Call(addMethod, [Expr.Coerce(list, typeof<obj>); deserializeField property field])
            else
                Expr.PropertySet(msgExpr, property.ProvidedProperty, deserializeField property field)
                
        let setRepeated property (var: Var) =
            let itemTy = var.Type.GenericTypeArguments.[0]
            let toListMethod = 
                <@@ toList x @@>
                |> Expr.getMethodDef
                |> Expr.makeGenericMethod [itemTy]

            let list = Expr.Call(toListMethod, [Expr.Coerce(Expr.Var(var), typeof<obj>)]) 
            Expr.PropertySet(msgExpr, property.ProvidedProperty, list)
        
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

        let createListMethodDef = <@@ createResizeArray<_>() @@> |> Expr.getMethodDef
        let createList ty = Expr.Call(Expr.makeGenericMethod [ty] createListMethodDef, [])
        let create ty = 
            let createMethod = <@@ create<_>() @@> |> Expr.getMethodDef |> Expr.makeGenericMethod [ty]
            Expr.Call(createMethod, [])
            
        let listsDefinitions = 
            listVars.Values
            |> Seq.fold 
                (fun acc var -> Expr.Let(var, create var.Type, acc))
                (fieldLoop :: setRepeatedFields @ [msgExpr] |> Expr.sequence)
                
//        Expr.Let(msgVar, Expr.create ty, listsDefinitions)
        Expr.Let(msgVar, Expr.create ty, msgExpr) // TODO Implement Expr.iterate using Expr.While and uncomment the line above
//        
//        with
//        | ex -> 
//            printfn "Failed to generate Deserialize method for type %s. Details: %O" ty.Name ex
//            reraise()

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