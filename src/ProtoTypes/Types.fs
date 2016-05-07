namespace ProtoTypes

open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
open Froto.Core

type TypeKind = 
    | Primitive
    | Class
    | Enum

type ProtoPropertyInfo = 
    { ProvidedProperty: ProvidedProperty;
      BackingField: ProvidedField;
      ProtoField: ProtoField; 
      TypeKind: TypeKind}

[<AbstractClass>]
type Message() as this =
 
    let mutable size = lazy (
        let buffer = NullWriteBuffer()
        this.Serialize buffer |> ignore
        buffer.Length
    )
 
    member this.SerializedLength = size.Value
    
    abstract Serialize: ZeroCopyBuffer -> ZeroCopyBuffer
    
    abstract ReadFrom: ZeroCopyBuffer -> ZeroCopyBuffer

type internal Dummy() = 
    inherit Message()
    
    override this.Serialize(buffer) = buffer
    override this.ReadFrom(buffer) = buffer