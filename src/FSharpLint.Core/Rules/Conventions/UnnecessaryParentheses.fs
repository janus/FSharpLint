module FSharpLint.Rules.UnnecessaryParentheses

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let private generateFix (text:string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let toText = fromText.TrimStart('(').TrimEnd(')')
        { FromText = fromText; FromRange = range; ToText = toText }))

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.IfThenElse(SynExpr.Paren(expression, _, _, range), _, _, _, _, _, _)) ->
        match expression with
        | SynExpr.App(_, _, _, _, _) -> Array.empty
        | _ ->
            { Range = range
              Message = Resources.GetString("RulesUnnecessaryParenthesesError")
              SuggestedFix = Some (generateFix args.FileContent range)
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
