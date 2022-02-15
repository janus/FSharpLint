module FSharpLint.Core.Tests.Rules.Conventions.DiscouragedSomeTypeMemberSuffixes

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsDiscouragedSomeTypeMemberSuffixes() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(DiscouragedSomeTypeMemberSuffixes.rule)

    [<Test>]
    member this.ThisShouldProduceError_1() =
        this.Parse """
module Person =
    type T = { First: string; CalledLst: string } """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_2() =
        this.Parse """
module Person =
    type T = { First: string; MainList: List<string> } """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_3() =
        this.Parse """
module Schools =
    type T = { CityList: List<string> } """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_4() =
        this.Parse """
type Tree =
    | Opt
    | NodeList of int * Tree * Tree """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_5() =
        this.Parse """
type Tree =
    | TreeOpt
    | Node of int * Tree * Tree """

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

    [<Ignore "WIP">]
    [<Test>]
    member this.ThisShouldNotProduceError_3() =
        this.Parse """
module Person =
    type T = { First: string; Last: string } """

        Assert.IsTrue this.NoErrorsExist
