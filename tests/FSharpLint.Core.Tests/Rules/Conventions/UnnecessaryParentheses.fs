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

    [<Test>]
    member this.``quick fix for single identifiers in elif expressions``() =
        let source = """
if foo then bar
elif (baz) then foobar
"""
        let expected = """
if foo then bar
elif baz then foobar
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``no quick fix because parentheses shouldn't be removed (1)``() =
        let source = """
if foo then (bar)
elif baz then foobar
"""
        let expected = """
if foo then (bar)
elif baz then foobar
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``no quick fix because parentheses shouldn't be removed (2)``() =
        let source = """
if foo then bar
elif baz then (foobar)
"""
        let expected = """
if foo then bar
elif baz then (foobar)
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``parentheses in discriminated unions are unnecessary (1)``() =
        this.Parse """
match foo with
| None -> ()
| Some(bar) -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses in discriminated unions are unnecessary (2)``() =
        this.Parse """
match foo with
| Something -> ()
| OtherThing(bar) -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses in discriminated unions are unnecessary (3)``() =
        this.Parse """
match foo with
| Something -> ()
| OtherThing(bar), baz -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses in discriminated unions should be kept (1)``() =
        this.Parse """
match foo with
| Something -> ()
| OtherThing (bar, baz) -> ()
"""
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``parentheses in discriminated unions should be kept (2)``() =
        this.Parse """
match foo with
| Something -> ()
| OtherThing (AndLastThing bar) -> ()
"""
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``parentheses in discriminated unions should be kept (3)``() =
        this.Parse """
match foo with
| Something -> ()
| OtherThing ({ Bar = Baz.FooBar } as fooBarBaz) -> ()
"""
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``quick fix for unnecessary parentheses in discriminated unions (1)``() =
        let source = """
match foo with
| None -> ()
| Some(bar) -> ()
"""
        let expected = """
match foo with
| None -> ()
| Some bar -> ()
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for unnecessary parentheses in discriminated unions (2)``() =
        let source = """
match foo with
| Something -> ()
| OtherThing(bar) -> ()
"""
        let expected = """
match foo with
| Something -> ()
| OtherThing bar -> ()
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for unnecessary parentheses in discriminated unions (3)``() =
        let source = """
match foo with
| Something -> ()
| OtherThing(bar), baz -> ()
"""
        let expected = """
match foo with
| Something -> ()
| OtherThing bar, baz -> ()
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``no quick fix because parentheses in discriminated unions should be kept (1)``() =
        let source = """
match foo with
| Something -> ()
| OtherThing (bar, baz) -> ()
"""
        let expected = """
match foo with
| Something -> ()
| OtherThing (bar, baz) -> ()
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``parentheses around lambda's parameters should be removed (1)``() =
        this.Parse "(fun (foo) -> bar foo) |> ignore"
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses around lambda's parameters should be removed (2)``() =
        this.Parse "(fun foo (bar) -> baz foo) |> ignore"
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses around lambda's parameters should be removed (3)``() =
        this.Parse """
module Foo =
    let sum (a) (b) = a + b
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses around lambda's parameters should be kept (1)``() =
        this.Parse "(fun (foo: bar) -> baz foo) |> ignore"
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``parentheses around lambda's parameters should be kept (2)``() =
        this.Parse "(fun (foo, bar) -> baz foo, baz bar) |> ignore"
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``parentheses around lambda's parameters should be kept (3)``() =
        this.Parse "(fun (foo: bar) (baz: foobar) -> foobarbaz foo baz) |> ignore"
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``parentheses in typed params should be kept``() =
        this.Parse """
module Foo =
    let sum (a: int) (b: int) = a + b
"""
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``quick fix for parentheses around lambda's parameters (1)``() =
        let source = "(fun (foo) -> bar foo) |> ignore"
        let expected = "(fun foo -> bar foo) |> ignore"
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for parentheses around lambda's parameters (2)``() =
        let source = "(fun foo (bar) -> baz foo) |> ignore"
        let expected = "(fun foo bar -> baz foo) |> ignore"
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for parentheses around lambda's parameters (3)``() =
        let source = """
module Foo =
    let sum (a) (b) = a + b
"""
        let expected = """
module Foo =
    let sum a (b) = a + b
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``no quick fix for parentheses in typed params``() =
        let source = """
module Foo =
    let sum (a: int) (b: int) = a + b
"""
        let expected = """
module Foo =
    let sum (a: int) (b: int) = a + b
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``parentheses in discriminated unions are unnecessary (4)``() =
        this.Parse """
match baz with
| Something -> ()
| OtherThing(_) -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses in discriminated unions are unnecessary (5)``() =
        this.Parse """
match baz with
| Something -> ()
| (_) -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``quick fix for parentheses in discriminated unions are unnecessary (4)``() =
        let source = """
match baz with
| Something -> ()
| OtherThing(_) -> ()
"""
        let expected = """
match baz with
| Something -> ()
| OtherThing _ -> ()
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for parentheses in discriminated unions are unnecessary (5)``() =
        let source = """
match baz with
| Something -> ()
| (_) -> ()
"""
        let expected = """
match baz with
| Something -> ()
| _ -> ()
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``parentheses in function call should be removed``() =
        this.Parse "raise(InvalidPassword)"
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``no parentheses in function call``() =
        this.Parse "raise InvalidPassword"
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``parentheses should be removed and left pipe introduced between arguments and function call``() =
        this.Parse "raise(AddressWithInvalidChecksum None)"
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses should be removed and left pipe introduced between argument(inner function call)  and function call``() =
        this.Parse """raise(Exception("foo"))"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``parentheses should be removed and left pipe introduced between argument(inner function call with two arguments)  and function call``() =
        this.Parse "raise(Exception(ex.ToString(), (ex.InnerException)))"
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``using the new keyword``() =
        this.Parse """
let CalculateSum (file: FileInfo) =
    if not (file.Exists) then
        raise (new FileNotFoundException("File not found", file.FullName))
    ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``quick fix for parentheses in function call``() =
        let source = "raise(InvalidPassword)"
        let expected = "raise InvalidPassword"
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``no quick fix for no parentheses in function call``() =
        let source = "raise InvalidPassword"
        let expected = "raise InvalidPassword"
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for between arguments and function call``() =
        let source = "raise(AddressWithInvalidChecksum None)"
        let expected = "raise <| AddressWithInvalidChecksum None"
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for between argument(inner function call) and function call``() =
        let source = """raise(Exception("foo"))"""
        let expected = """raise <| Exception("foo")"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for between argument(inner function call with two arguments) and function call``() =
        let source = "raise(Exception(ex.ToString(), (ex.InnerException)))"
        let expected = "raise <| Exception(ex.ToString(), (ex.InnerException))"
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for using the new keyword``() =
        let source = """
let CalculateSum (file: FileInfo) =
    if not (file.Exists) then
        raise (new FileNotFoundException("File not found", file.FullName))
    ()
"""
        let expected = """
let CalculateSum (file: FileInfo) =
    if not (file.Exists) then
        raise  <| new FileNotFoundException("File not found", file.FullName)
    ()
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``remove unneeded parentheses in match clause if tuple has only one element and also not DU``() =
        this.Parse """
match foo with
| (Bar) -> ()
| _ -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``keep parentheses in match clause if tuple is a DU``() =
        this.Parse """
match foo with
| (Bar baz) -> ()
| _ -> ()
"""
        Assert.IsTrue this.NoErrorsExist
