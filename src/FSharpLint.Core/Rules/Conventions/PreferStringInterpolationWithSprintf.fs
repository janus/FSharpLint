module FSharpLint.Rules.PreferStringInterpolationWithSprintf

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let mutable identifiers = Set.empty

let private isStringFormat (identifiers: List<Ident>) =
    "String" = identifiers.[0].idText && "Format" = identifiers.[1].idText

let runner args =
    match args.AstNode with
    | AstNode.Expression(SynExpr.App(_, _, SynExpr.LongIdent(_, LongIdentWithDots(ids, _), _, _), paren, range)) when ids.Length = 2 && isStringFormat ids ->
        match paren with
        | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.Const(SynConst.String(_, _, _), _); _], _, _), _, _, _) ->
            { Range = range
              Message = Resources.GetString "RulesPreferStringInterpolationWithSprintf"
              SuggestedFix = None
              TypeChecks = List.empty }
            |> Array.singleton
        | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.Ident identifier; _], _, _), _, _, range) when identifiers.Contains identifier.idText ->
            { Range = range
              Message = Resources.GetString "RulesPreferStringInterpolationWithSprintf"
              SuggestedFix = None
              TypeChecks = List.empty }
            |> Array.singleton
        | _ -> Array.empty
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, SynPat.Named(_, identifier, _, _, _), _, SynExpr.Const(SynConst.String(value, _, _), _), range, _)) when value.Contains "{0}" ->
        identifiers <- identifiers.Add(identifier.idText)
        Array.empty
    | AstNode.ModuleOrNamespace _ ->
        identifiers <- Set.empty
        Array.empty
    | _ -> Array.empty

let cleanup () = identifiers <- Set.empty

let rule =
    { Name = "PreferStringInterpolationWithSprintf"
      Identifier = Identifiers.PreferStringInterpolationWithSprintf
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = cleanup } }
    |> AstNodeRule
