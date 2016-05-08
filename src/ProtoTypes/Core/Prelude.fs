namespace ProtoTypes.Core

open System
open System.IO

open Printf

[<AutoOpen>]
module Prelude =

    let notsupportedf fmt = ksprintf (NotSupportedException >> raise) fmt

    let (</>) path1 path2 = Path.Combine(path1, path2)

    let (+.+) scope1 scope2 = (scope1 + "." + scope2).Trim('.')

    let x<'T> : 'T = Unchecked.defaultof<'T>

    let notNull x = not <| isNull x