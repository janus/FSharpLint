module FSharpLint.Core.Tests.Rules.Conventions.StringFormating

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsStringFormating() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(StringFormating.rule)

    [<Test>]
    member this.StringFormatingWithSprintfShouldNotProduceError() =
        this.Parse """
let someString = sprintf "Hello %s" world"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.StringFormatingWithStringFormatShouldNotProduceError() =
        this.Parse """
let someString = String.Format(someTemplate, world)"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.StringFormatingWithStringFormatShouldProduceError() =
        this.Parse """
let someString = String.Format("Hello {0}", world)"""

        Assert.IsTrue this.ErrorsExist
