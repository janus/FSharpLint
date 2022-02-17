module FSharpLint.Core.Tests.Rules.Conventions.AvoidTypeHintSuffixesForNames

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsAvoidTypeHintSuffixesForNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidTypeHintSuffixesForNames.rule)

    [<Test>]
    member this.ThisShouldProduceError_1() =
        this.Parse """
module Person =
    type FullName = { FirstName: string; SurnamesList: List<string> } """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_2() =
        this.Parse """
type Tree =
    | Scalar
    | NodeList of int * Tree * Tree """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_3() =
        this.Parse """
type MyClass() =
    let random  = new System.Random()
    member val RandomOpt = (random.Next() |> Some) with get, set """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_4() =
        this.Parse """
type MyClass() =
   let mutable someString = "someString"
   member this.SomeStr with get() = someString and set(v : string) = someString <- v """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisNotShouldProduceError_1() =
        this.Parse """
type Tree =
    | Tip
    | Node of int * Tree * Tree """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ThisShouldNotProduceError_2() =
        this.Parse """
type Tree =
    | Opt
    | Node of int * Tree * Tree """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ThisShouldNotProduceError_3() =
        this.Parse """
module Person =
    type FullName = { First: string; Last: string } """

        Assert.IsTrue this.NoErrorsExist
