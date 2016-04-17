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

let msg = p :> ProtoTypes.Message

let buffer = ZeroCopyBuffer(1000)
msg.Serialize(buffer)

buffer


// open Microsoft.FSharp.Quotations
// open Microsoft.FSharp.Quotations.Patterns

// let getMethod = function
//     | Call(_,  m, _) ->
//         if m.IsGenericMethod then m.GetGenericMethodDefinition() else m
//     | _ -> failwithf "expression is not supported"
    
// let x<'T> : 'T = Unchecked.defaultof<'T>

// let foo a b c = printfn "Hello %s %i %A" a b c
    
    
// let e = <@@ foo "123" 123 123 @@>
// let ec = <@@ foo "123" 123 @@>
// let f = <@@ (%%ec: int -> unit)(123) @@>
// let def = <@@ foo x x x @@> |> getMethod
// f.Type.ToString()

open Froto.Core
open ProtoTypes
open Microsoft.FSharp.Quotations

type ConcreteMessage() =
    inherit Message() with
        override __.Serialize(buffer) = buffer
        
let genericWrite = Serialization.writeEmbeddedMethod.MakeGenericMethod(typeof<ConcreteMessage>)

let value = Var("value", typeof<ConcreteMessage>)
let valueExpr = Expr.Var(value)
let positionExpr = Expr.Value(1)
let buffer = Expr.Var(Var("buffer", typeof<ZeroCopyBuffer>))
Expr.Lambda(value, Expr.Call(genericWrite, [positionExpr; buffer; valueExpr]))

// let m = Serialization.writeOptionalMethod
// let t = typeof<ConcreteMessage>

// let gener = m.MakeGenericMethod(t)
// let buffer = Expr.Var(Var("buffer", typeof<ZeroCopyBuffer>))
// let value = Expr.Var(Var("value", typeof<ConcreteMessage option>))
// let writer = <@@ Serialization.writeEmbedded 1 %%buffer @@>
// Microsoft.FSharp.Quotations.Expr.Call(gener, [writer; value])