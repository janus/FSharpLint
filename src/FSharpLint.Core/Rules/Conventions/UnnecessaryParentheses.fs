module FSharpLint.Rules.UnnecessaryParentheses

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.IfThenElse(SynExpr.Paren (_, _, _, _), _, _, _, _, _, range)) ->
        { Range = range
          Message = Resources.GetString("RulesUnnecessaryParenthesesError")
          SuggestedFix = None
          TypeChecks = List.Empty }
        |> Array.singleton
    | _ -> Array.empty

let rule =
    { Name = "UnnecessaryParentheses"
      Identifier = Identifiers.UnnecessaryParentheses
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
