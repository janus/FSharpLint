module FSharpLint.Rules.AvoidTypeHintSuffixesForNames

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let ruleName: string = "AvoidTypeHintSuffixesForNames"
let discouragedMemberSuffixes: List<string> = ["Lst"; "List"; "Array"; "Opt"; "Str"]

let checkRecordFields (fields: List<SynField>) =
    let rec traverse recordFields (identifiers: List<string>) =
        match recordFields with
        | SynField(_, _, maybeVal, synType, _, _, _, _)::rest ->
            match maybeVal with
            | Some field ->
                let identifier: string = field.idText
                let likelySuffixes = discouragedMemberSuffixes |> List.filter (fun text -> not (identifier.Equals text))
                if likelySuffixes |> List.exists identifier.EndsWith then
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
        | SynUnionCase(_, ident, _, _, _, _)::rest ->
                let identifier: string = ident.idText
                let likelySuffixes = discouragedMemberSuffixes |> List.filter (fun text -> not (identifier.Equals text))
                if likelySuffixes |> List.exists identifier.EndsWith then
                    traverse rest (identifier::identifiers)
                else
                    traverse rest identifiers
        | _ -> identifiers

    traverse fields List.empty

let isTypeHintSuffixesinProperties (members: List<SynMemberDefn>) =
    let rec traverse memberDefinitions =
        match memberDefinitions with
        | SynMemberDefn.AutoProperty(_, _, ident, _, _, _, _, _, expression, _, _)::rest ->
                let identifier: string = ident.idText
                let likelySuffixes = discouragedMemberSuffixes |> List.filter (fun text -> not (identifier.Equals text))
                if likelySuffixes |> List.exists identifier.EndsWith then
                    true
                else
                    traverse rest
        | _::rest -> traverse rest
        | [] -> false

    traverse members

let runner args =
    match args.AstNode with
    | TypeDefinition(SynTypeDefn(_, typeRepresentation, _members, _implicitCtor, range)) ->
        match typeRepresentation with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
            let identifiers = checkRecordFields fields
            match identifiers with
            | head::_ ->
               let error =
                   { Range = range
                     Message = Resources.GetString ruleName
                     SuggestedFix = None
                     TypeChecks = List.Empty }
                   |> Array.singleton
               error
            | [] -> Array.empty
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_, fields, _), _) ->
            let identifiers = checkUnionFields fields
            match identifiers with
            | head::_ ->
               let error =
                   { Range = range
                     Message = Resources.GetString ruleName
                     SuggestedFix = None
                     TypeChecks = List.Empty }
                   |> Array.singleton
               error
            | [] -> Array.empty
        | SynTypeDefnRepr.ObjectModel(_, members, _) ->
            if isTypeHintSuffixesinProperties members then
               let error =
                   { Range = range
                     Message = Resources.GetString ruleName
                     SuggestedFix = None
                     TypeChecks = List.Empty }
                   |> Array.singleton
               error
            else
                Array.empty         
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = ruleName
      Identifier = Identifiers.AvoidTypeHintSuffixesForNames
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
