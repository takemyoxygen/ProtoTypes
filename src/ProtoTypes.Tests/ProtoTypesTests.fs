﻿[<NUnit.Framework.TestFixture>]
module ProtoTypes.Tests

open System

open NUnit.Framework
open FsUnit

open Froto.Core

open ProtoTypes
open ProtoTypes.Core

type Proto = ProtocolBuffersTypeProvider<"proto/person.proto">
type Sample = Proto.ProtoTypes.Sample

let private createPerson() =
    let address = 
        Sample.Person.Address(
            Address1 = "Street", 
            HouseNumber = 12, 
            Whatever = [1; 2; 3], 
            SomeInts = [Sample.Person.IntContainer(Value = 5); Sample.Person.IntContainer(Value = 7)])

    Sample.Person(
        Name = "Name",
         Id = 1,
         HasCriminalConvictions = false,
         Weight = 82.3, 
         PersonGender = Sample.Person.Gender.Female, 
         Email = Some "Email", 
         PersonAddress = Some address)

let serializeDeserialize<'T when 'T :> Message> (msg: 'T) (deserialize: ZeroCopyBuffer -> 'T) =
    let buffer = ZeroCopyBuffer 1000
    msg.Serialize buffer |> ignore
    
    let buffer' = ZeroCopyBuffer buffer.AsArraySegment
    deserialize buffer'

[<Test>]
let ``Person test``() =
    let person = createPerson()
    person.Name |> should be (equal "Name")
    person.PersonGender |> should be (equal Sample.Person.Gender.Female)
    person.PersonAddress.Value.Address1 |> should be (equal "Street")
    
    
[<Test>]
let ``Serialization test``() =
    let person = createPerson()
    
    let buffer = ZeroCopyBuffer 1000
    person.Serialize buffer |> ignore
    buffer.Position |> should be (greaterThan 0)

[<Test>]
let ``Deserialization test``() =
    let person = createPerson()
    let person' = serializeDeserialize person Sample.Person.Deserialize
    
    person'.Name |> should be (equal person.Name)
    person'.Id |> should be (equal person.Id)
    person'.HasCriminalConvictions |> should be (equal person.HasCriminalConvictions)
    person'.Weight |> should be (equal person.Weight)
    person'.PersonGender |> should be (equal person.PersonGender)
    person'.Email |> should be (equal person.Email)
    
    person'.PersonAddress.IsSome |> should be True
    let address = person.PersonAddress.Value
    let address' = person'.PersonAddress.Value
    address'.Address1 |> should be (equal address.Address1)
    address'.HouseNumber |> should be (equal address.HouseNumber)
    address'.Whatever |> should be (equal address.Whatever)
    address'.SomeInts |> List.map (fun v -> v.Value) |> should be (equal (address.SomeInts |> List.map(fun v -> v.Value)))
    
    
[<Test>]
let ``Deserialize None optional value``() =
    let person = createPerson()
    person.PersonAddress <- None
    let person' = serializeDeserialize person Sample.Person.Deserialize
    
    person'.PersonAddress.IsSome |> should be False
    

[<Test>]
let ``Deserialize empty repeated value``() =
    let person = createPerson()
    let address = person.PersonAddress.Value
    
    address.SomeInts <- []
    address.Whatever <- []

    let address' = serializeDeserialize address Sample.Person.Address.Deserialize
    
    address'.SomeInts |> should be Empty
    address'.Whatever |> should be Empty