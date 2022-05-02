module FSharpLint.Rules.FavourBasicControlFlow

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System


let runner args =
    match args.AstNode with
    | _ -> Array.empty

let rule =
    { Name = "FavourBasicControlFlow"
      Identifier = Identifiers.FavourBasicControlFlow
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
