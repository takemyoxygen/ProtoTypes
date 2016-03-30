# ProtoTypes
F# type provider for Google Protocol Buffers

Looks like currently there's no type providers for [Google Protocol Buffers](https://developers.google.com/protocol-buffers/). One might eventually appear in this repository.

Some links:

1. Related discussion in [FSharpx.Extras repository](https://github.com/fsprojects/FSharpx.Extras/issues/124)
2. [FSharpx.Extras branch](https://github.com/fsprojects/FSharpx.Extras/tree/protobuf) where attempt to sketch a type provider was made
3. [Froto](https://github.com/ctaggart/froto) - a tool that parses and generates code from `.proto` files. Parsing files and creating ASTs might be re-used in type provider implementation. 

Plan so far:

1. Implement simple generation of types based on [Froto.Parser](https://www.nuget.org/packages/Froto.Parser/0.2.0-b030). Most likely it will be a set of immutable types with public constructors.
2. Implement serialization/deserialization to/from byte array.
3. See what's next.