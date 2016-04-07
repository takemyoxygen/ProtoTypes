#I "../../build"

#r "ProtoTypes.dll"
#r "FParsecCS.dll"
#r "FParsec.dll"
#r "Froto.Parser.dll"


[<Literal>]
let path = __SOURCE_DIRECTORY__ + "/../ProtoTypes.Tests/proto/person.proto"
type ProtoBuf = ProtoTypes.ProtocolBuffersTypeProvider<path>

type Person = ProtoBuf.Person
Person.Address
let address = Person.Address("Foo", 123, [1; 2; 3])
let p = Person("Name", 123, false, 102.1, Person.Gender.Male, Some "Email",  Some address)

Person.Gender.Male
