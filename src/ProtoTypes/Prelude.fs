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