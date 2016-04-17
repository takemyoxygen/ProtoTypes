namespace ProtoTypes

open Microsoft.FSharp.Quotations

open Froto.Parser.Model
open Froto.Core
open Froto.Core.Encoding

/// Contains methods to serialize supported data types to binary format.
/// So far, this module is a simple wrapper around Froto.Core.Encoding.Serializer.dehydrateXXX family of inlined functions.
/// Such wrapper is needed, because apprently erased type provides don't support direct calls of inlined functions.
module Serialization = 

    let writeInt32 fieldNumber (value: int) buffer =
        Serializer.dehydrateVarint fieldNumber value buffer |> ignore
        
    let writeString fieldNumber value buffer =
        Serializer.dehydrateString fieldNumber value buffer |> ignore
        
    let writeBool fieldNumber value buffer =
        Serializer.dehydrateBool fieldNumber value buffer |> ignore
        
    let writeDouble fieldNumber value buffer =
        Serializer.dehydrateDouble fieldNumber value buffer |> ignore
        
    /// Serializes optional field using provided function to handle inner value if present
    /// e.g. writeOptional 1 (Some "abc") buffer writeString
    let writeOptional (fieldNumber: int) value (buffer: ZeroCopyBuffer) writeInner =
        match value with
        | Some(v) -> writeInner fieldNumber v buffer
        | None -> ()

    /// Serializes nested message. Uses provided "embed" function to serialize
    /// nested message content
    let writeEmbedded fieldNumber (size: uint64) buffer embed = 
        buffer
        |> WireFormat.encodeTag fieldNumber WireType.LengthDelimited
        |> WireFormat.encodeVarint size
        |> ignore
        
        embed buffer
        
    let serialize (prop: ProtoPropertyInfo) buffer this =
        let value = Expr.FieldGet(this, prop.BackingField)
        let position = prop.ProtoField.Position
        match prop.TypeKind with
        | Primitive -> 
            match prop.ProtoField.Type with
            | "int32" when prop.ProtoField.Rule = Required -> 
                Expr.Application(<@@ writeInt32 position %%value @@>, buffer)
            | _ -> Expr.Value(())
            
        | x ->
            Expr.Value(()) 
            //notsupportedf "Type kind '%A' is not supported" x
