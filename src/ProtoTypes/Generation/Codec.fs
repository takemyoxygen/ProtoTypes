namespace ProtoTypes.Generation

open System

open Froto.Core
open Froto.Core.Encoding

open ProtoTypes.Core

type Writer<'T> = int -> ZeroCopyBuffer -> 'T -> unit
type Reader<'T> = RawField -> 'T

/// Contains helper functions to read/write values to/from ZeroCopyBuffer
[<RequireQualifiedAccess>]
module Codec =

    let write f (fieldNumber: int) (buffer: ZeroCopyBuffer) value =
        f fieldNumber value buffer |> ignore 

    let writeInt32: Writer<int> = write Serializer.dehydrateVarint
    let writeString: Writer<string> = write Serializer.dehydrateString
    let writeBool: Writer<bool> = write Serializer.dehydrateBool
    let writeDouble: Writer<float> = write Serializer.dehydrateDouble
    let writeFloat32: Writer<float32> = fun _ -> notsupportedf "float32 is currently not supported"
    let writeInt64: Writer<int64> = write Serializer.dehydrateVarint
    let writeUInt32: Writer<uint32> = write Serializer.dehydrateVarint
    let writeUInt64: Writer<uint64> = write Serializer.dehydrateVarint
    let writeSInt32: Writer<int> = write Serializer.dehydrateSInt32
    let writeSInt64: Writer<int64> = write Serializer.dehydrateSInt64
    let writeFixed32: Writer<int> = write Serializer.dehydrateFixed32
    // TODO Serializer.dehydrateFixed64 should accept int64. Should be fixed in Froto
    let writeFixed64: Writer<int64> = fun f -> fun b -> fun v -> Serializer.dehydrateFixed64 f (int v) b |> ignore
    let writeSFixed32: Writer<int> = fun _ -> notsupportedf "sfixed32 is currently not supported"
    let writeSFixed64: Writer<int64> = fun _ -> notsupportedf "sfixed64 is currently not supported"
    let writeBytes: Writer<ArraySegment<byte>> = write Serializer.dehydrateBytes
        
    /// Serializes optional field using provided function to handle inner value if present
    let writeOptional writeInner value =
        match value with
        | Some(v) -> writeInner v
        | None -> ()
        
    /// Value is expected to be of type option<'T>. It's not possible
    /// to use this type directly in the signature because of type providers limitations.
    /// All optional non-generated types (i.e. primitive types and enums) should be serialized using
    /// more strongly-typed writeOptional function
    let writeOptionalEmbedded<'T when 'T :> Message> (writeInner: Message -> unit) (value: obj) =
        if value <> null 
        then value :?> option<'T> |> Option.get |> writeInner
        
    let writeRepeated writeItem values =
        for value in values do writeItem value
        
    let writeRepeatedEmbedded<'T when 'T :> Message> (writeInner: Message -> unit) (value: obj) =
        value :?> list<'T> |> List.iter writeInner

    let writeEmbedded fieldNumber buffer (message: Message) = 
        buffer
        |> WireFormat.encodeTag fieldNumber WireType.LengthDelimited
        |> WireFormat.encodeVarint (uint64 message.SerializedLength)
        |> message.Serialize
        |> ignore 

    let decodeFields (zcb: ZeroCopyBuffer) = seq {
        while (not zcb.IsEof) && zcb.Array.[int zcb.Position] > 7uy do
            yield WireFormat.decodeField zcb
    }
    
    let private readField<'T> f field = 
        let result = ref Unchecked.defaultof<'T>
        f result field
        !result

    let deserialize<'T when 'T :> Message and 'T : (new: unit -> 'T)> buffer =
        let x = new 'T()
        x.ReadFrom buffer |> ignore
        x

    let readDouble: Reader<float> = readField Serializer.hydrateDouble
    let readFloat: Reader<float32> = fun _ -> notsupportedf "float32 is currently not supported"
    let readInt32: Reader<int> = readField Serializer.hydrateInt32
    let readInt64: Reader<int64> = readField Serializer.hydrateInt64
    let readUInt32: Reader<uint32> = readField Serializer.hydrateUInt32
    let readUInt64: Reader<uint64> = readField Serializer.hydrateUInt64
    let readSInt32: Reader<int> = readField Serializer.hydrateSInt32
    let readSInt64: Reader<int64> = readField Serializer.hydrateSInt64
    let readFixed32: Reader<uint32> = readField Serializer.hydrateFixed32
    let readFixed64: Reader<uint64> = readField Serializer.hydrateFixed64
    let readSFixed32: Reader<int> = readField Serializer.hydrateSFixed32
    let readSFixed64: Reader<int64> = readField Serializer.hydrateSFixed64
    let readBool: Reader<bool> = readField Serializer.hydrateBool
    let readString: Reader<string> = readField Serializer.hydrateString
    let readBytes: Reader<byte[]> = readField Serializer.hydrateBytes 
    
    let readEmbedded<'T when 'T :> Message and 'T : (new: unit -> 'T)> field = 
        match field with
        | LengthDelimited(_, segment) -> ZeroCopyBuffer segment |> deserialize<'T>
        | _ -> failwithf "Invalid format of the field: %O" field