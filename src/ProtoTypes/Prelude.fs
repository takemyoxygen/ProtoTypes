namespace ProtoTypes

open System
open System.Collections.Generic
open System.IO

[<AutoOpen>]
module Prelude =

    open Printf
    
    let notsupportedf fmt = 
        ksprintf (NotSupportedException >> raise) fmt

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

    open System.Collections
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

    let private isGenerated (ty: Type) =
        ty :? ProvidedTypeDefinition || (ty.IsGenericType && ty.GetGenericArguments() |> Seq.exists (fun gt -> gt :? ProvidedTypeDefinition))

    let makeGenericMethod (types: Type list) methodInfo =
        if types |> List.exists isGenerated
        then ProvidedTypeBuilder.MakeGenericMethod(methodInfo, types)
        else methodInfo.MakeGenericMethod(types |> Array.ofList)

    let makeGenericType (types: Type list) typeDef =
        if types |> List.exists isGenerated
        then ProvidedTypeBuilder.MakeGenericType(typeDef, types)
        else typeDef.MakeGenericType(types |> Array.ofList)

    let rec private typeHierarchy (ty: Type) = seq {
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
        let iterVar = Var("x", elementType)
        // let bodyExpr = Expr.Lambda(itetVar, body <| Expr.Var(itetVar))
        
        let enumeratorVar = Var("enumerator", typedefof<IEnumerator<_>> |> makeGenericType [elementType])
        let enumeratorExpr = Expr.Var enumeratorVar
        
        let moveNextMethod = typeof<IEnumerator>.GetMethod("MoveNext")
        let disposeMethod = typeof<IDisposable>.GetMethod("Dispose")
        
        let whileLoop = 
            Expr.WhileLoop(
                Expr.Call(enumeratorExpr, moveNextMethod, []),
                Expr.Let(
                    iterVar, 
                    Expr.PropertyGet(enumeratorExpr, enumeratorVar.Type.GetProperty("Current")), 
                    body <| Expr.Var iterVar))
        
        // Expr.TryFinally is not really supported by FSharp.TypeProviders.StarterPack
        // so, Expr.Sequential is used insted (Dispose() won't be called if exception is raised)
        Expr.Let(
            enumeratorVar, 
            Expr.Call(sequence, sequence.Type.GetMethod("GetEnumerator"), []),
            Expr.Sequential(whileLoop, Expr.Call(enumeratorExpr, disposeMethod, [])))

    let create (ty: Type) = 
        let ctor = 
            if ty :? ProvidedTypeDefinition then
                ty.GetConstructors()
                |> Seq.find(fun c -> c :? ProvidedConstructor && c.GetParameters() |> Array.isEmpty)
            else ty.GetConstructor([||])
        Expr.NewObject(ctor, [])