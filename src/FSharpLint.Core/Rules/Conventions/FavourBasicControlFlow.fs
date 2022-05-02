module FSharpLint.Rules.FavourBasicControlFlow

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private isBasicControlFlow (synMatchClauses: List<SynMatchClause>) =
  let isFirstNamedIdentifier firstClause =
    match firstClause with
    | SynMatchClause(SynPat.Named(_, _, _, _, _), _, _, _, _) -> true
    | _ -> false

  let isLastTarget lastClause =
    match lastClause with
    | SynMatchClause(SynPat.Wild _, None, _, _, _) -> true
    | _ -> false

  synMatchClauses.Length = 2 && isFirstNamedIdentifier synMatchClauses.[0] && isLastTarget synMatchClauses.[1]

let runner args =
  match args.AstNode with
  | AstNode.Expression(SynExpr.Match (_, _, synMatchClauses, range)) when isBasicControlFlow synMatchClauses ->
    { Range = range
      Message = Resources.GetString "FavourBasicControlFlow"
      SuggestedFix = None
      TypeChecks = List.empty }
    |> Array.singleton
  | _ ->
    Array.empty

let rule =
  { Name = "FavourBasicControlFlow"
    Identifier = Identifiers.FavourBasicControlFlow
    RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
  |> AstNodeRule
