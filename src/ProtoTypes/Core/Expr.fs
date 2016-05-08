namespace ProtoTypes.Core

open System
open System.Collections
open System.Collections.Generic

open FSharp.Quotations
open FSharp.Quotations.Patterns

open ProviderImplementation.ProvidedTypes

[<RequireQualifiedAccess>]
module Expr =

    let sequence: seq<Expr> -> Expr = Seq.reduce (fun acc s -> Expr.Sequential(acc, s))

    let getMethodDef = function
        | Call(_, m, _) ->
            if m.IsGenericMethod
            then m.GetGenericMethodDefinition()
            else m
        | x -> notsupportedf "Expression %A is not supported" x

    let private isGenerated (ty: Type) =
        ty :? ProvidedTypeDefinition || 
        (ty.IsGenericType && ty.GetGenericArguments() |> Seq.exists (fun gt -> gt :? ProvidedTypeDefinition))

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
    let forLoop (sequence: Expr) (body: Expr -> Expr) =
        let elementType = 
            typeHierarchy sequence.Type
            |> Seq.tryFind (fun ty -> ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<seq<_>>)
            |> Option.map (fun ty -> ty.GetGenericArguments().[0])
            |> Option.require "Given collection is not a seq<'T>"
            
        let iterVar = Var("x", elementType)
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

    let callStatic parameters staticMethod = Expr.Call(staticMethod, parameters)