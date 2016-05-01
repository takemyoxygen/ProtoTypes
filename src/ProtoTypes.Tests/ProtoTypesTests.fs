[<NUnit.Framework.TestFixture>]
module ProtoTypes.Tests

open System

open NUnit.Framework
open FsUnit

open Froto.Core

type Proto = ProtoTypes.ProtocolBuffersTypeProvider<"proto/person.proto">
type Sample = Proto.ProtoTypes.Sample

[<Test>]
let ``Person test``() =
    let address = Sample.Person.Address(Address1 = "Street", HouseNumber = 12, Whatever = [1; 2; 3], SomeInts = [Sample.Person.IntContainer(Value = 5); Sample.Person.IntContainer(Value = 7)])
    let p = Sample.Person(Name = "Name", Id = 1, HasCriminalConvictions = false, Weight = 82.3, PersonGender = Sample.Person.Gender.Female, Email = Some "Email", PersonAddress = Some address)
    p.Name |> should be (equal "Name")
    p.PersonGender |> should be (equal Sample.Person.Gender.Female)
    p.PersonAddress.Value.Address1 |> should be (equal "Street")
    
[<Test>]
let ``Serialization test``() =
    let address = Sample.Person.Address(Address1 = "Street", HouseNumber = 12, Whatever = [1; 2; 3], SomeInts = [Sample.Person.IntContainer(Value = 5); Sample.Person.IntContainer(Value = 7)])
    let person = Sample.Person(Name = "Name", Id = 1, HasCriminalConvictions = false, Weight = 82.3, PersonGender = Sample.Person.Gender.Female, Email = Some "Email", PersonAddress = Some address)
    
    let buffer = ZeroCopyBuffer(1000)
    person.Serialize(buffer) |> ignore
    
    buffer.Position |> should be (greaterThan 0)
    