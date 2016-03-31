namespace ProtoTypes

open System

[<AutoOpen>]
module Prelude =
    
    let notsupportedf format args = 
        sprintf format args
        |> NotSupportedException
        |> raise