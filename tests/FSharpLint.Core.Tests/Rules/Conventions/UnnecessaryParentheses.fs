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

    [<Test>]
    member this.``no quick fix because we prefer the rule to be conservative``() =
        let source = "if (foo foobar) then bar else baz"

        let expected = "if (foo foobar) then bar else baz"

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``parentheses around single identifiers in if expressions are unnecessary``() =
        this.Parse """
if (foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo) then
    bar else baz
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses around single identifiers in elif expressions are unnecessary (1)``() =
        this.Parse """
if foo then bar
elif (baz) then foobar
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses shouldn't be removed (1)``() =
        this.Parse """
if foo then (bar)
elif baz then foobar
"""
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``parentheses shouldn't be removed (2)``() =
        this.Parse """
if foo then bar
elif baz then (foobar)
"""
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``parentheses around single identifiers in elif expressions are unnecessary (2)``() =
        this.Parse """
if fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo then bar
elif (bazzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz) then foobar
"""
        Assert.IsTrue this.ErrorsExist
