module FSharpLint.Rules.StringFormating

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let private isStringFormat (identifiers: List<Ident>) =
    "String" = identifiers.[0].idText && "Format" = identifiers.[1].idText

let runner args =
    match args.AstNode with
    | AstNode.Expression(SynExpr.App(_, _, SynExpr.LongIdent(_, LongIdentWithDots(ids, _), _, _), paren, range)) when ids.Length = 2 && isStringFormat ids ->
        match paren with
        | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.Const(SynConst.String(_, _, _), _); _], _, _), _, _, _) ->
            { Range = range
              Message = Resources.GetString "RulesStringFormating"
              SuggestedFix = None
              TypeChecks = List.empty }
            |> Array.singleton
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = "StringFormating"
      Identifier = Identifiers.StringFormating
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
