namespace ProtoTypes.Generation

open ProtoTypes.Core
open ProviderImplementation.ProvidedTypes

open Froto.Parser.Model
    
type TypesLookup = Map<string, TypeKind * ProvidedTypeDefinition>

module internal TypesRegistry = 
        
    let private getShortName (fullName: string) = fullName.Split('.') |> Seq.last
    
    let rec private allScopes (scope: string) = seq{
        yield scope
        let lowestScopePosition = scope.LastIndexOf(".")
        if lowestScopePosition > 0 
        then yield! scope.Substring(0, lowestScopePosition) |> allScopes
    } 

    let discoverTypes scope (messages: ProtoMessage seq) =
    
        let rec loop scope (message: ProtoMessage) = seq {
            let fullName = scope +.+ message.Name
            yield Class, fullName
            yield! message.Enums |> Seq.map (fun enum -> Enum, fullName +.+ enum.Name)
            yield! message.Messages |> Seq.collect (loop fullName)
        }
        
        messages
        |> Seq.collect (loop scope)
        |> Seq.map (fun (kind, fullName) ->
            let name = getShortName fullName
            let ty =  
                match kind with
                | Class -> Provided.message name
                | Enum -> Provided.enum name
                | x -> invalidOp <| sprintf "Type represending %O should not be discovered" x
            fullName, (kind, ty))
        |> Map.ofSeq
        
    let resolve scope targetType (lookup: TypesLookup) = 
        allScopes scope
        |> Seq.map (fun s -> s +.+ targetType)
        |> Seq.map (fun tp -> lookup |> Map.tryFind tp)
        |> Seq.tryFind Option.isSome
        |> Option.unwrap
