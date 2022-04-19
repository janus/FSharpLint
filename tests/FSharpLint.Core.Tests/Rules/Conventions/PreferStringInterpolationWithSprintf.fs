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
let someTemplate = "Hello {0}"
let someString = String.Format(someTemplate, world)"""

        Assert.IsTrue this.ErrorsExist


    [<Test>]
    member this.StringInterpolationWithMultipleModuleWithSameVariableNameNotShouldProduceError() =
        this.Parse """
module Foo =
    let someTemplate = "Hello, this is not for String.Format actually {0}"
module Bar =
    let someFunction someTemplate =
        Console.WriteLine(String.Format(someTemplate, "world"))"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.StringInterpolationWithSameVariableNameInMultipleLetNotShouldProduceError() =
        this.Parse """
module Bar =
    let exampleFunction =
        let someTemplate = "Hello, this is not for String.Format actually {0}"
        someTemplate
    let fooFunction () =
        89
    let someFunction someTemplate =
        let returnConstInt () =
            89
        let label = "Test"
        Console.WriteLine(String.Format(someTemplate, "world"))"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.StringInterpolationWithSameVariableNameinInneLetNotShouldProduceError() =
        this.Parse """
module Bar =
    let exampleFunction someTemplate =
        let someResults =
            let someTemplate = "Hello, this is not for String.Format actually {0}"
            someTemplate
        Console.WriteLine(String.Format(someTemplate, "world"))"""

        Assert.IsTrue this.NoErrorsExist
