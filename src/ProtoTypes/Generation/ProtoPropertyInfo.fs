namespace ProtoTypes.Generation

open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model

type TypeKind = 
    | Primitive
    | Class
    | Enum

type ProtoPropertyInfo = 
    { ProvidedProperty: ProvidedProperty;
      BackingField: ProvidedField;
      ProtoField: ProtoField; 
      TypeKind: TypeKind}