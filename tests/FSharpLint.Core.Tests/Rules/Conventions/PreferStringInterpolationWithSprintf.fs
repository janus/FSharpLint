module FSharpLint.Core.Tests.Rules.Conventions.PreferStringInterpolationWithSprintf

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsPreferStringInterpolationWithSprintf() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PreferStringInterpolationWithSprintf.rule)

    [<Test>]
    member this.StringInterpolationWithSprintfShouldNotProduceError() =
        this.Parse """
let someString = sprintf "Hello %s" world"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.StringInterpolationWithStringFormatShouldProduceError() =
        this.Parse """
let someString = String.Format("Hello {0}", world)"""

        Assert.IsTrue this.ErrorsExist


    [<Test>]
    member this.StringInterpolationWithStringFormatAndExternalTemplateShouldNotProduceError() =
        this.Parse """
let someFunction someTemplate =
    Console.WriteLine(String.Format(someTemplate, world))"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.StringInterpolationWithStringFormatAndLocalVariableShouldProduceError() =
        this.Parse """
let someTemplate = "Hello %s"
let someString = String.Format(someTemplate, world)"""

        Assert.IsTrue this.ErrorsExist
