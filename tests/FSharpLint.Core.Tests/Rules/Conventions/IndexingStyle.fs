module FSharpLint.Core.Tests.Rules.Conventions.IndexingStyle

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsIndexingStyleOCaml() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(IndexingStyle.rule { Style="ocaml" })

    [<Test>]
    member this.IndexingStyleOCaml() =
        this.Parse """
let someArray = [| "foo" ; "bar" |]
let bar = someArray.[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.NoErrorsExist

    member this.IndexingStyleCSharpShouldProduceError() =
        this.Parse """
let someArray = [| "foo" ; "bar" |]
let bar = someArray[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.NoErrorsExist

type TestConventionsIndexingStyleCSharp() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(IndexingStyle.rule { Style="csharp" })

    [<Test>]
    member this.IndexingStyleCSharp() =
        this.Parse """
let someArray = [| "foo" ; "bar" |]
let bar = someArray[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.NoErrorsExist

   [<Test>]
    member this.IndexingStyleOCamlShouldProduceError() =
        this.Parse """
let someArray = [| "foo" ; "bar" |]
let bar = someArray.[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.ErrorsExist
