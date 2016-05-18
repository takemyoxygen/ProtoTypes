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
    
// type ProtoBuf = ProtoTypes.ProtocolBuffersTypeProvider<path>
// type Sample = ProtoBuf.ProtoTypes.Sample
// let address = Sample.Person.Address(Address1 = "Street", HouseNumber = 12, Whatever = [1; 2; 3], SomeInts = [Sample.Person.IntContainer(Value = 5); Sample.Person.IntContainer(Value = 7)])
// let p = Sample.Person(Name = "Name", Id = 1, HasCriminalConvictions = false, Weight = 82.3, PersonGender = Sample.Person.Gender.Female, Email = Some "Email", PersonAddress = Some address)

// let buffer = ZeroCopyBuffer(1000)

// p.Serialize(buffer)

// let buffer2 = ZeroCopyBuffer(buffer.AsArraySegment)

// let p2 = Sample.Person.Deserialize buffer2
// p2.Weight

let model = Froto.Parser.Model.ProtoFile.ParseFile path

let msg = model.Messages |> List.last
msg.Parts

open Froto.Parser.Model
open Froto.Parser.Ast

type ProtoMessageMember =
    | Field of rule: Froto.Parser.Model.ProtoFieldRule * fieldType: string * name: string * position: uint32
    | OneOfGroup of name: string
    | OneOfMember of group: string
    | Enum of name: string * values: list<string * int>
    | Message of name: string 
    
let mapRule: PLabel -> ProtoFieldRule = function
    | TRequired -> Required
    | TRepeated -> Repeated
    | TOptional -> Optional
    
let mapType: PType -> string = function
    | TIdent name -> name
    | x -> x.ToString().ToLower().[1..]

model.Messages.Head.Parts
|> Seq.collect (fun part -> seq {
    match part with
    | TField(name, rule, ty, position, _) -> yield Field(mapRule rule, mapType ty, name, position)
    | TMessageEnum(name, statements) -> 
        let values = statements |> List.choose (function | TEnumOption _ -> None | TEnumField(name, position, _) -> Some(name, position))
        yield Enum(name, values)
    | _ -> ()
    })