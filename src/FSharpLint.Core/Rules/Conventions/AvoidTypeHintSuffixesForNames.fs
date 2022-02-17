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

let isTypeHintSuffixesInRecordFields (fields: List<SynField>) =
    let rec traverse recordFields =
        match recordFields with
        | SynField(_, _, maybeVal, synType, _, _, _, _)::rest ->
            match maybeVal with
            | Some field ->
                let identifier: string = field.idText
                let likelySuffixes = discouragedMemberSuffixes |> List.filter (fun text -> not (identifier.Equals text))
                if likelySuffixes |> List.exists identifier.EndsWith then
                    true
                else
                    traverse rest
            | None ->
                traverse rest
        | [] -> false

    traverse fields

let isTypeHintSuffixesInUnionFields (fields: List<SynUnionCase>) =
    let rec traverse unionCases =
        match unionCases with
        | SynUnionCase(_, ident, _, _, _, _)::rest ->
            let identifier: string = ident.idText
            let likelySuffixes = discouragedMemberSuffixes |> List.filter (fun text -> not (identifier.Equals text))
            if likelySuffixes |> List.exists identifier.EndsWith then
                true
            else
                traverse rest
        | [] -> false

    traverse fields

let isTypeHintSuffixesInProperties (members: List<SynMemberDefn>) =
    let rec traverse memberDefinitions =
        match memberDefinitions with
        | SynMemberDefn.AutoProperty(_, _, ident, _, _, _, _, _, _expression, _, _)::rest ->
            let identifier: string = ident.idText
            let likelySuffixes = discouragedMemberSuffixes |> List.filter (fun text -> not (identifier.Equals text))
            if likelySuffixes |> List.exists identifier.EndsWith then
                true
            else
                traverse rest
        | SynMemberDefn.Member(SynBinding(_, _, _, _, _, _, _, pattern, _, expression, _, _), _)::rest ->
            match pattern with
            | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
                let identifier: string =  ident.Tail.Head.idText
                let likelySuffixes = discouragedMemberSuffixes |> List.filter (fun text -> not (identifier.Equals text))
                if likelySuffixes |> List.exists identifier.EndsWith then
                    true
                else
                    traverse rest
            | _ -> traverse rest
        | _::rest -> traverse rest
        | [] -> false

    traverse members

let runner args =
    match args.AstNode with
    | TypeDefinition(SynTypeDefn(_, typeRepresentation, _members, _implicitCtor, range)) ->
        match typeRepresentation with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
            if isTypeHintSuffixesInRecordFields fields then
               let error =
                   { Range = range
                     Message = Resources.GetString ruleName
                     SuggestedFix = None
                     TypeChecks = List.Empty }
                   |> Array.singleton
               error
            else
                Array.empty
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_, fields, _), _) ->
            if isTypeHintSuffixesInUnionFields fields then
               let error =
                   { Range = range
                     Message = Resources.GetString ruleName
                     SuggestedFix = None
                     TypeChecks = List.Empty }
                   |> Array.singleton
               error
            else
                Array.empty
        | SynTypeDefnRepr.ObjectModel(_, members, _) ->
            if isTypeHintSuffixesInProperties members then
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
