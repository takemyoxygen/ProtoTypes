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
type Message() =
 
    abstract SerializedLength: int
    
    abstract Serialize: ZeroCopyBuffer -> unit