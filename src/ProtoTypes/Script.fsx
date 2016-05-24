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
let address = Sample.Person.Address(Address1 = "Street", HouseNumber = 12, Whatever = [1; 2; 3], SomeInts = [Sample.Person.IntContainer(Value = 5); Sample.Person.IntContainer(Value = 7)])
let p = Sample.Person(Name = "Name", Id = 1, HasCriminalConvictions = false, Weight = 82.3, PersonGender = Sample.Person.Gender.Female, Email = Some "Email", PersonAddress = Some address)

let buffer = ZeroCopyBuffer(1000)

let container = Sample.MapContainer()
container.People

open ProtoTypes.Core
open ProtoTypes.Generation
<@@ Codec.writeRepeated x x x x@@>
type Fun<'T> = int -> string -> 'T -> unit

let foo<'T> : Fun<'T> = fun x y z -> ()

<@@ foo @@>