module FSharpLint.Rules.IndexingStyle

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System


[<RequireQualifiedAccess>]
type Config = { Style: string }

let runner (config: Config) args =
    let style = config.Style
    match args.AstNode with
    | _ -> Array.empty

let rule config =
    { Name = "IndexingSytle"
      Identifier = Identifiers.IndexingStyle
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
