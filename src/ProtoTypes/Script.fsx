#I "../../build"

#r "ProtoTypes.dll"
#r "FParsecCS.dll"
#r "FParsec.dll"
#r "Froto.Parser.dll"


[<Literal>]
let path = __SOURCE_DIRECTORY__ + "/../../proto/person.proto"
type ProtoBuf = ProtoTypes.ProtocolBuffersTypeProvider<path>

type Person = ProtoBuf.Person
let p = Person("Name", 123, Some "Email")

p.Email