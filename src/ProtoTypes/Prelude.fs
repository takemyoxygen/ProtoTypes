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