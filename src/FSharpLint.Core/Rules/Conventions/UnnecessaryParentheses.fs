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
        let mutable toText = fromText.TrimStart('(').TrimEnd(')')
        if fromText.IndexOf('(', 0, fromText.Length) > 0 then
            toText <- toText.Replace('(', ' ')
        { FromText = fromText; FromRange = range; ToText = toText }))

let private traversePattern patterns text =
    let rec loop patterns warnings =
        match patterns with
        | SynPat.LongIdent(LongIdentWithDots([identifier], _), _, _,SynArgPats.Pats([SynPat.Paren(SynPat.Named(SynPat.Wild _, _, _, _, _), _)]), _, range) :: rest ->
            let warning = { Range = range
                            Message = Resources.GetString("RulesUnnecessaryParenthesesError")
                            SuggestedFix = Some (generateFix text range)
                            TypeChecks = List.Empty }
            loop rest (warning :: warnings)
        | _ :: rest -> loop rest warnings
        | [] -> List.toArray warnings

    loop patterns List.Empty

let private traverseLambdaPattern patterns text =
    let rec loop patterns =
        match patterns with
        | SynPat.Paren(SynPat.Named(SynPat.Wild _, _, _, _, _), range) :: _ ->
            { Range = range
              Message = Resources.GetString("RulesUnnecessaryParenthesesError")
              SuggestedFix = Some (generateFix text range)
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
    | AstNode.Match(SynMatchClause(SynPat.LongIdent(LongIdentWithDots(_, _), _, _,SynArgPats.Pats([SynPat.Paren(SynPat.Named(SynPat.Wild _, _, _, _, _), _)]), _, range), _, _, _, _))
    | AstNode.Match(SynMatchClause(SynPat.LongIdent(LongIdentWithDots(_, _), _, _,SynArgPats.Pats([SynPat.Paren(SynPat.Wild _, _)]), _, range), _, _, _, _)) ->
        { Range = range
          Message = Resources.GetString("RulesUnnecessaryParenthesesError")
          SuggestedFix = Some (generateFix args.FileContent range)
          TypeChecks = List.Empty }
        |> Array.singleton
    | AstNode.Match(SynMatchClause(SynPat.Tuple(_, patterns, _), _, _, _, _)) ->
        traversePattern patterns args.FileContent
    | AstNode.Expression(SynExpr.Lambda(_, _, _, _, Some(patterns, _), _)) ->
        traverseLambdaPattern patterns args.FileContent
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(LongIdentWithDots(_, _), _, _,SynArgPats.Pats(patterns), _, _), _, _, _, _)) ->
        traverseLambdaPattern patterns args.FileContent
    | AstNode.Match(SynMatchClause(SynPat.Paren(SynPat.Wild _, range), _, _, _, _)) ->
        { Range = range
          Message = Resources.GetString("RulesUnnecessaryParenthesesError")
          SuggestedFix = Some (generateFix args.FileContent range)
          TypeChecks = List.Empty }
        |> Array.singleton
    | AstNode.Expression(SynExpr.App(_, _, identifier, SynExpr.Paren(_, _, _, range), _)) ->
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
