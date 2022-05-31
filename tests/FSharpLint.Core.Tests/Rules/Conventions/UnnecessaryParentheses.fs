module FSharpLint.Core.Tests.Rules.Conventions.UnnecessaryParentheses

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsUnnecessaryParentheses() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UnnecessaryParentheses.rule)

    [<Test>]
    member this.``no unnecessary parentheses in the if conditional expression``() =
        this.Parse "if foo then bar else baz"
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``unnecessary parentheses in the if conditional expression``() =
        this.Parse "if (foo) then bar else baz"
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``prefer conservative rule which means unnecessary parentheses is not flagged for function application``() =
        this.Parse "if (foo foobar) then bar else baz"
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``quick fix for unnecessary parentheses.``() =
        let source = "if (foo) then bar else baz"

        let expected = "if foo then bar else baz"

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

