# ProtoTypes
F# type provider for Google Protocol Buffers

Looks like currently there's no type providers for [Google Protocol Buffers](https://developers.google.com/protocol-buffers/). One might eventually appear in this repository.

Some links:

1. Related discussion in [FSharpx.Extras repository](https://github.com/fsprojects/FSharpx.Extras/issues/124)
2. [FSharpx.Extras branch](https://github.com/fsprojects/FSharpx.Extras/tree/protobuf) where attempt to sketch a type provider was made
3. [Froto](https://github.com/ctaggart/froto) - a tool that parses and generates code from `.proto` files. Parsing files and creating ASTs might be re-used in type provider implementation. 

Current state:

1. Generative type provider with basic support of generating types from single `.proto` file, serialization and deserialization.
2. Supports `proto2` syntax only. `required` fields are represented as regular properties, `optional` fields are mapped to properties of type `option<'T>`, and `repeated` fields correspond to `list<'T>` properties.
3. Supports embedded messages, and enums. List of supported scalar types includes `int32`, `string`, `bool` and `double`

As disscussed [here](https://github.com/ctaggart/froto/issues/3), this repository migth eventually become part of Froto.
