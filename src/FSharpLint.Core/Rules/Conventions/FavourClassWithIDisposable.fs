module FSharpLint.Rules.FavourClassWithIDisposable

open System
open System.Linq
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules


let private runner (args:AstNodeRuleParams) =
    match (args.AstNode, args.CheckInfo) with
    | AstNode.Expression(SynExpr.New(_, synType, expression, range)), Some checkInfo ->
        // Use checkInfo to infer if new expression implemented IDisposable
        Array.empty

    | _ -> Array.empty

let rule =
    { Name = "FavourClassWithIDisposable"
      Identifier = Identifiers.FavourClassWithIDisposable
      RuleConfig = { AstNodeRuleConfig.Runner = runner
                     Cleanup = ignore } }
    |> AstNodeRule
