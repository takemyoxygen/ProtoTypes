[<NUnit.Framework.TestFixture>]
module ProtoTypes.Tests

open NUnit.Framework
open FsUnit

type PersonProto = ProtoTypes.ProtocolBuffersTypeProvider<"proto/person.proto">

[<Test>]
let ``Person test``() =
    let p = PersonProto.Person("Name", 1, false, 82.3, Some "Email")
    p.Name |> should be (equal "Name")