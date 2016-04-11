#I "../../build"

#r "ProtoTypes.dll"
#r "FParsecCS.dll"
#r "FParsec.dll"
#r "Froto.Parser.dll"
#r "Froto.Core.dll"

open Froto.Core
open Froto.Core.Encoding

[<Literal>]
let path = __SOURCE_DIRECTORY__ + "/../ProtoTypes.Tests/proto/person.proto"
type ProtoBuf = ProtoTypes.ProtocolBuffersTypeProvider<path>.ProtoTypes.Sample

type Person = ProtoBuf.Person
let address = Person.Address("Foo", 123, [1; 2; 3])
let p = Person("Name", 123, false, 102.1, Person.Gender.Male, Some "Email",  Some address)
