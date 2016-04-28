namespace ProtoTypes

open Microsoft.FSharp.Quotations

open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

/// Contains methods to serialize supported data types to binary format.
/// So far, this module is a simple wrapper around Froto.Core.Encoding.Serializer.dehydrateXXX family of inlined functions.
/// Such wrapper is needed, because apprently erased type provides don't support direct calls of inlined functions.
module Serialization = 

    let writeInt32 fieldNumber buffer (value: int) =
        Serializer.dehydrateVarint fieldNumber value buffer |> ignore
        
    let writeString fieldNumber buffer value =
        Serializer.dehydrateString fieldNumber value buffer |> ignore
        
    let writeBool fieldNumber buffer value =
        Serializer.dehydrateBool fieldNumber value buffer |> ignore
        
    let writeDouble fieldNumber buffer value =
        Serializer.dehydrateDouble fieldNumber value buffer |> ignore
        
    /// Serializes optional field using provided function to handle inner value if present
    let writeOptional writeInner value =
        match value with
        | Some(v) -> writeInner v
        | None -> ()
        
    let writeOptionalEmbedded<'T when 'T :> Message> (writeInner: Message -> unit) (value: obj) =
        if value <> null 
        then value :?> option<'T> |> Option.get |> writeInner
        
    let writeRepeated writeItem values =
        for value in values do writeItem value
        
    let writeRepeatedEmbedded<'T when 'T :> Message> (writeInner: Message -> unit) (value: obj) =
        value :?> list<'T> |> List.iter writeInner

    /// Serializes nested message. Uses provided "embed" function to serialize
    /// nested message content
    let writeEmbedded fieldNumber buffer (message: Message) = 
        buffer
        |> WireFormat.encodeTag fieldNumber WireType.LengthDelimited
        |> WireFormat.encodeVarint (uint64 message.SerializedLength)
        |> message.Serialize
        |> ignore 
    
    let x<'T> : 'T = Unchecked.defaultof<'T>
    let writeEmbeddedMethodDef = <@@ writeEmbedded x x x @@> |> Expr.getMethodDef
    let writeOptionalMethodDef = <@@ writeOptional x x @@> |> Expr.getMethodDef
    let writeRepeatedMethod = <@@ writeRepeated x x @@> |> Expr.getMethodDef
    let writeOptionalEmbeddedMethodDef = <@@ writeOptionalEmbedded x x @@> |> Expr.getMethodDef
    let writeRepeatedEmbeddedMethodDef = <@@ writeRepeatedEmbedded x x @@> |> Expr.getMethodDef
        
    let serialize (prop: ProtoPropertyInfo) buffer this =
        let value = Expr.FieldGet(this, prop.BackingField)
        
        let underlyingType = 
            if prop.BackingField.FieldType.IsGenericType
            then prop.BackingField.FieldType.GetGenericArguments().[0]
            else prop.BackingField.FieldType
            
        let position = prop.ProtoField.Position
        
        // writer is an expression that represents a function 'T -> unit
        let writer =
            match prop.TypeKind with
                | Primitive -> 
                    match prop.ProtoField.Type with
                    | "int32" -> <@@ writeInt32 position %%buffer@@>
                    | "string" -> <@@ writeString position %%buffer @@>
                    | "double" -> <@@ writeDouble position %%buffer @@>
                    | "bool" -> <@@ writeBool position %%buffer @@>
                    | x -> notsupportedf "Primitive type '%s' is not supported" x 
                | Class -> <@@ writeEmbedded position %%buffer @@>
                | Enum -> <@@ writeInt32 position %%buffer @@>
        
        try
            match prop.ProtoField.Rule with
            | Required -> Expr.Application(writer, value)
            | Optional ->
                match prop.TypeKind with
                | Class ->
                    let writeOptional = ProvidedTypeBuilder.MakeGenericMethod(writeOptionalEmbeddedMethodDef, [underlyingType])
                    Expr.Call(writeOptional, [writer; Expr.Coerce(value, typeof<obj>)])
                | _ -> 
                    let writeOptional = writeOptionalMethodDef.MakeGenericMethod(underlyingType)
                    Expr.Call(writeOptional, [writer; value])
            | Repeated ->
                match prop.TypeKind with
                | Class ->
                    let writeRepeated = ProvidedTypeBuilder.MakeGenericMethod(writeRepeatedEmbeddedMethodDef, [underlyingType])
                    Expr.Call(writeRepeated, [writer; Expr.Coerce(value, typeof<obj>)])
                | _ ->
                    let writeRepeated = writeRepeatedMethod.MakeGenericMethod(underlyingType)
                    Expr.Call(writeRepeated, [writer; value])
        with
        | ex -> 
            printfn "Failed for property %s: %O. Error: %O" prop.ProvidedProperty.Name value.Type ex
            reraise()
