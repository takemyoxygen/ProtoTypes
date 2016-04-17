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
    
type ProtoBuf = ProtoTypes.ProtocolBuffersTypeProvider<path>
type Sample = ProtoBuf.ProtoTypes.Sample
let address = Sample.Person.Address("Foo", 123, [1; 2; 3])

let p = Sample.Person("Name", 123, false, 102.1, Sample.Person.Gender.Male, Some "Email",  Some address)

let buffer = ZeroCopyBuffer(1000)
p.Serialize(buffer)

buffer