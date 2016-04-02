#I "../../build"

#r "ProtoTypes.dll"
#r "FParsecCS.dll"
#r "FParsec.dll"
#r "Froto.Parser.dll"

open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
    
type ProtoBuf = ProtoTypes.ProtocolBuffersTypeProvider<"proto/person.proto">
type Person = ProtoBuf.Person
let p = Person("Name", 123, Some "Email")
p.Email
p.Id
p.Name
type Container = Dictionary<string, obj>

let vars = 
    [Var("varInt", typeof<int>); Var("varString", typeof<string>); Var("varSomeString", typeof<string option>)]
    |> List.map Expr.Var

let addMethod = typeof<Container>.GetMethod("Add")

let callAdd dict name value =
    Expr.Call(dict, addMethod, [Expr.Value(name); Expr.Coerce(value, typeof<obj>)])

let rec add dict values =
    match values with
    | [(name, value)] -> callAdd dict name value
    | (name, value)::tail -> Expr.Sequential(callAdd dict name value, add dict tail)
    | [] -> Expr.Value(())
    
let addSequentially dict vars = 
    let namedArgs =
        vars
        |> List.map (fun var ->
            match var with
            | Var(v) -> v.Name, var
            | x -> failwithf "Expression %+A is not supported" x)
    add dict namedArgs
    
let var = Var("d", typeof<Container>)
let varExp = Expr.Var var 
let body = Expr.Sequential(addSequentially varExp vars, Expr.Value(varExp))
Expr.Let(var, Expr.NewObject(typeof<Container>.GetConstructor([||]), []), body)
let xx = 
    <@@ 
        let x = new Dictionary<string, obj>()
        x.Add("foo1", 1)
        x.Add("foo2", 2)
        x.Add("foo3", 3)
    @@>