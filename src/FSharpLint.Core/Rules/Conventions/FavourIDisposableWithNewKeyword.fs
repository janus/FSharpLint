module FSharpLint.Rules.FavourIDisposableWithNewKeyword

open System
open System.Linq
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols


let private runner (args:AstNodeRuleParams) =
    Array.empty

let rule =
    { Name = "FavourIDisposableWithNewKeyword"
      Identifier = Identifiers.FavourIDisposableWithNewKeyword
      RuleConfig = { AstNodeRuleConfig.Runner = runner
                     Cleanup = ignore } }
    |> AstNodeRule
