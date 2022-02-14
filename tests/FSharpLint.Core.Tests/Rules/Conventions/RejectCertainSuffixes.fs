module FSharpLint.Core.Tests.Rules.Conventions.RejectCertainSuffixes

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsRejectCertainSuffixes() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(RejectCertainSuffixes.rule)

    [<Test>]
    member this.ThisShouldProduceError_1() =
        this.Parse """
module Person =
    type T = {First:string; Last:string} with
        member this.nameStr =
            this.First + " " + this.Last
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_2() =
        this.Parse """
module Schools =
    type T = {listofCities:List<string>; arrayofSchools:List<string>} with
        member this.cityList =
            this.listofCities
                       
        member this.schoolArray =
            this.arrayofSchools """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ThisShouldProduceError_3() =
        this.Parse """
type Tree =
    | Opt
    | NodeList of int * Tree * Tree """

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
type private Tree =
    | Opt
    | NodeList of int * Tree * Tree """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ThisShouldNotProduceError_3() =
        this.Parse """
module Person =
    type private T = {First:string; Last:string} with
        member this.nameStr =
            this.First + " " + this.Last
"""

        Assert.IsTrue this.NoErrorsExist
