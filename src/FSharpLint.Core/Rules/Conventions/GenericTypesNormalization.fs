module FSharpLint.Rules.GenericTypesNormalization

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | _ -> Array.empty

let rule =
    { Name = "GenericTypesNormalization"
      Identifier = Identifiers.GenericTypesNormalization
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
