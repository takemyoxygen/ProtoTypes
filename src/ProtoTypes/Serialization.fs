namespace ProtoTypes

open Microsoft.FSharp.Quotations

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
        
    let writeRepeated writeItem values =
        for value in values do writeItem value

    /// Serializes nested message. Uses provided "embed" function to serialize
    /// nested message content
    let writeEmbedded fieldNumber buffer (message: #Message) = 
        buffer
        |> WireFormat.encodeTag fieldNumber WireType.LengthDelimited
        |> WireFormat.encodeVarint (uint64 message.SerializedLength)
        |> message.Serialize
        |> ignore 
    
    let x<'T> : 'T = Unchecked.defaultof<'T>
    let writeOptionalMethod = <@@ writeOptional x x @@> |> Expr.getMethodDef
    let writeRepeatedMethod = <@@ writeRepeated x x @@> |> Expr.getMethodDef
    let writeEmbeddedMethod = <@@ writeEmbedded x x x @@> |> Expr.getMethodDef
        
    let serialize (prop: ProtoPropertyInfo) buffer this =
        let value = Expr.FieldGet(this, prop.BackingField)
        
        let undelryingType = 
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
                | Class -> 
                    let value = Var("value", undelryingType)
                    let valueExpr = Expr.Var(value)
                    let genericWrite = writeEmbeddedMethod.MakeGenericMethod(undelryingType)
                    let positionExpr = Expr.Value(position)
                    let writer = Expr.Lambda(value, Expr.Call(genericWrite, [positionExpr; buffer; valueExpr]))
                    writer
                | Enum -> <@@ writeInt32 position %%buffer @@>
        
        match prop.ProtoField.Rule with
        | Required -> Expr.Application(writer, value)
        | Optional ->
            Expr.Call(
                writeOptionalMethod.MakeGenericMethod(undelryingType),
                [writer; value])
        | Repeated ->
            Expr.Call(
                writeRepeatedMethod.MakeGenericMethod(undelryingType),
                [writer; value])
