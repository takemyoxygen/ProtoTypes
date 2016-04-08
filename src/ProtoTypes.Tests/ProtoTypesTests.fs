[<NUnit.Framework.TestFixture>]
module ProtoTypes.Tests

open NUnit.Framework
open FsUnit

type Proto = ProtoTypes.ProtocolBuffersTypeProvider<"proto/person.proto">.ProtoTypes.Sample

[<Test>]
let ``Person test``() =
    let address = Proto.Person.Address("Street", 12, [1; 3; 14])
    let p = Proto.Person("Name", 1, false, 82.3, Proto.Person.Gender.Female, Some "Email", Some address)
    p.Name |> should be (equal "Name")
    p.PersonGender |> should be (equal Proto.Person.Gender.Female)
    p.PersonAddress.Value.Address1 |> should be (equal "Street")
    