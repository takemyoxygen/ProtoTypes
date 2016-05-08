namespace ProtoTypes.Generation

open Froto.Core
open Froto.Core.Encoding

open ProtoTypes.Core

[<RequireQualifiedAccess>]
module Codec =

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

    let readString = readField Serializer.hydrateString
    
    let readInt = readField Serializer.hydrateInt32
    
    let readBool = readField Serializer.hydrateBool
    
    let readDouble = readField Serializer.hydrateDouble
    
    let readEmbedded<'T when 'T :> Message and 'T : (new: unit -> 'T)> field = 
        match field with
        | LengthDelimited(_, segment) -> ZeroCopyBuffer segment |> deserialize<'T>
        | _ -> failwithf "Invalid format of the field: %O" field