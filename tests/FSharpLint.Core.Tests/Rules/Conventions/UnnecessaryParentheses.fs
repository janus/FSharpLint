module FSharpLint.Core.Tests.Rules.Conventions.UnnecessaryParentheses

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsUnnecessaryParentheses() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UnnecessaryParentheses.rule)

    [<Test>]
    member this.``no unnecessary parentheses in the if conditional expression should not throw``() =
        this.Parse """
module Program

let dog =
    if true then
        "bar"
    else
        "baz"
"""
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``unnecessary parentheses in the if conditional expression should throw``() =
        this.Parse """
module Program

let dog =
    if (foo) then
        "bar"
    else
        "baz"
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``unnecessary parentheses in the else if conditional expression should throw``() =
        this.Parse """
module Program

let dog =
    if foo then
        "bar"
    else if (ball) then
        "baz"
    else
        "bet"
"""

        Assert.IsTrue this.ErrorsExist
