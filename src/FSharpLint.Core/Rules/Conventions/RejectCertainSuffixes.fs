module FSharpLint.Rules.RejectCertainSuffixes

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let blackListed: List<string> = ["Lst"; "Last"; "List"; "Array"; "Opt"; "Str"]

let checkRecordFields (fields: List<SynField>) =
    let rec traverse recordFields (identifiers: List<string>) =
        match recordFields with
        | SynField(_, _, maybeVal, synType, _, _, _, _)::rest ->
            match maybeVal with
            | Some field ->
                let identifier: string = field.idText
                if blackListed |> List.exists identifier.EndsWith then
                    traverse rest (identifier::identifiers)
                else
                    traverse rest identifiers
            | None ->
                traverse rest identifiers
        | _ -> identifiers

    traverse fields List.empty

let checkUnionFields (fields: List<SynUnionCase>) =
    let rec traverse unionCases (identifiers: List<string>) =
        match unionCases with
        | SynUnionCase(_, ident,_, _, _,_)::rest ->
                let identifier: string = ident.idText
                if blackListed |> List.exists identifier.EndsWith then
                    traverse rest (identifier::identifiers)
                else
                    traverse rest identifiers
        | _ -> identifiers

    traverse fields List.empty

let runner args =
    match args.AstNode with
    | TypeDefinition(SynTypeDefn(SynComponentInfo(_,_,_,_,_,_,accessibility,_), typeRepresentation, _members, _implicitCtor, _)) ->
        match accessibility with
        | Some SynAccess.Public | None ->
            match typeRepresentation with
            | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
                checkRecordFields fields |> ignore
                Array.empty
            | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_, fields, _), _) ->
                checkUnionFields fields |> ignore
                Array.empty
            | _ -> Array.empty
            
        | _ ->
            Array.empty
    | _ -> Array.empty

let rule =
    { Name = "RejectCertainSuffixes"
      Identifier = Identifiers.RejectCertainSuffixes
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
