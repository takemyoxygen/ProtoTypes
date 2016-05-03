namespace ProtoTypes

open System
open System.IO

[<AutoOpen>]
module Prelude =
    
    let notsupportedf format args = 
        sprintf format args
        |> NotSupportedException
        |> raise
        
    let (</>) path1 path2 = 
        Path.Combine(path1, path2)
        
    let (+.+) scope1 scope2 = (scope1 + "." + scope2).Trim('.')
    
    let trace x =
        printfn "%A" x
        x

    let x<'T> : 'T = Unchecked.defaultof<'T>

    let notNull = isNull >> not


[<RequireQualifiedAccess>]
module Option =

    /// Invokes function if provided option is None, otherwise returns original value of the provided option
    let otherwise f opt =
        match opt with
        | None -> f()
        | x -> x 
        
    /// If provided option is Some - it's value is returned, otherwise an exception with provided error message is thrown
    let require msg = function
        | Some(x) -> x
        | None -> failwith msg
        
    let getOrElse alternative = function
        | Some(x) -> x
        | None -> alternative
        
    let unwrap = function
        | Some(Some(x)) -> Some x
        | _ -> None
        
        
[<RequireQualifiedAccess>]
module Expr =

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    open ProviderImplementation.ProvidedTypes

    let sequence: seq<Expr> -> Expr = Seq.reduce (fun acc s -> Expr.Sequential(acc, s))
        
    let getMethodDef = function
        | Call(_, m, _) ->
            if m.IsGenericMethod
            then m.GetGenericMethodDefinition()
            else m
        | x -> notsupportedf "Expression %A is not supported" x

    let makeGenericMethod (types: Type list) methodInfo =
        if types |> List.exists (fun t -> t :? ProvidedTypeDefinition || (t.IsGenericType && t.GetGenericArguments() |> Seq.exists (fun gt -> gt :? ProvidedTypeDefinition)))
        then ProvidedTypeBuilder.MakeGenericMethod(methodInfo, types)
        else methodInfo.MakeGenericMethod(types |> Array.ofList)

    let rec typeHierarchy (ty: Type) = seq {
        if notNull ty
        then
            yield ty
            yield! typeHierarchy ty.BaseType
    }
    
    /// Generates an expression that iterates over a given sequence using provided body expression
    let iterate (sequence: Expr) (body: Expr -> Expr) =
        let elementType = 
            typeHierarchy sequence.Type
            |> Seq.tryFind (fun ty -> ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<seq<_>>)
            |> Option.map (fun ty -> ty.GetGenericArguments().[0])
            |> Option.require "Given collection is not a seq<'T>"
            
        let iterMethod = <@@ Seq.iter x x @@> |> getMethodDef |> makeGenericMethod [elementType]
        let itetVar = Var("x", elementType)
        let bodyExpr = Expr.Lambda(itetVar, body <| Expr.Var(itetVar))
        
        Expr.Call(iterMethod, [bodyExpr; sequence])

    let create (ty: Type) = 
        let ctor = 
            if ty :? ProvidedTypeDefinition then
                ty.GetConstructors()
                |> Seq.find(fun c -> c :? ProvidedConstructor && c.GetParameters() |> Array.isEmpty)
            else ty.GetConstructor([||])
        Expr.NewObject(ctor, [])