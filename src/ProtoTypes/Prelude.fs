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

    let seq = function
        | [] -> Expr.Value(())
        | h::[] -> h
        | h::t -> List.fold (fun acc e -> Expr.Sequential(acc, e)) h t
        
    let getMethodDef = function
        | Call(_, m, _) ->
            if m.IsGenericMethod
            then m.GetGenericMethodDefinition()
            else m
        | x -> notsupportedf "Expression %A is not supported" x