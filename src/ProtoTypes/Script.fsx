#I "../../build"
#r "ProtoTypes.dll"
#r "FParsecCS.dll"
#r "FParsec.dll"
#r "Froto.Parser.dll"
    
type ProtoBuf = ProtoTypes.ProtocolBuffersTypeProvider<"proto/person.proto">
type Person = ProtoBuf.Person
let p = new Person()
// open System.IO
// open Froto.Parser
// open Froto.Parser.Model

// let file = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "proto", "person.proto") 
// let proto = ProtoFile.ParseFile file

// let person = proto.Messages |> Seq.head 
// let fields = person.Fields |> Seq.map (fun field -> field.Name, field.Rule)