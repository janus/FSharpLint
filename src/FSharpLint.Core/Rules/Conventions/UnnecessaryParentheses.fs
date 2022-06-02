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

let private traversePattern patterns =

    let rec loop patterns =
        match patterns with
        | SynPat.LongIdent(LongIdentWithDots([identifier], _), _, _,SynArgPats.Pats([SynPat.Paren(SynPat.Named(SynPat.Wild _, _, _, _, range), _)]), _, _) :: _ ->
            { Range = range
              Message = Resources.GetString("RulesUnnecessaryParenthesesError")
              SuggestedFix = None
              TypeChecks = List.Empty }
            |> Array.singleton
        | _ :: rest -> loop rest
        | [] -> Array.empty

    loop patterns

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
    | AstNode.Match(SynMatchClause(SynPat.LongIdent(LongIdentWithDots(_, _), _, _,SynArgPats.Pats([SynPat.Paren(SynPat.Named(SynPat.Wild _, _, _, _, _), _)]), _, _), _, _, range, _)) ->
        { Range = range
          Message = Resources.GetString("RulesUnnecessaryParenthesesError")
          SuggestedFix = None
          TypeChecks = List.Empty }
        |> Array.singleton
    | AstNode.Match(SynMatchClause(SynPat.Tuple(_, patterns, _), _, _, _, _)) ->
        traversePattern patterns
    | _ -> Array.empty

let rule =
    { Name = "UnnecessaryParentheses"
      Identifier = Identifiers.UnnecessaryParentheses
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
