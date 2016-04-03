﻿namespace ProtoTypes

open System

[<RequireQualifiedAccess>]
module internal Naming =

    let private withFirstChar f (input: string) =
        if String.IsNullOrEmpty input then input
        else
            let first = input.[0] |> f |> string
            if input.Length > 1 then first + input.[1..]
            else first

    /// Converts "name_like_that" to "NameLikeThat"
    let snakeToPascal (identifier: string) =
        identifier.Split('_')
        |> Seq.map (withFirstChar Char.ToUpper)
        |> String.concat String.Empty

    /// Converts "NameLikeThat" to "nameLikeThat"
    let pascalToCamel = withFirstChar Char.ToLower