module FSharpLint.Core.Tests.Rules.Conventions.InternalValuesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.CamelCase
      Underscores = Some NamingUnderscores.AllowPrefix
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsInternalValuesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(InternalValuesNames.rule config)

    [<Test>]
    member this.InternalVariableIsCamelCase() =
        this.Parse """
module Program
  let internal cat = 1"""

        this.AssertNoWarnings()

    [<Test>]
    member this.InternalVariableIsPascalCase() =
        this.Parse """
module Program
  let internal Cat = 1"""

        Assert.IsTrue(this.ErrorExistsAt(3,15))

    [<Test>]
    member this.PublicVariableIsNotReported() =
        this.Parse """
module Program
  let Cat = 1"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PascalCaseLetBindingInTypeIsNotReported() =
        this.Parse """
module program
  let Cat() = ()"""

        this.AssertNoWarnings()

let pascalCaseConfig =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = None
      Prefix = None
      Suffix = None }

[<TestFixture>]
type TestConventionsPascalCase() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(InternalValuesNames.rule pascalCaseConfig)

    [<Test>]
    member this.``camelCase should be flagged because it is configured as PascalCase``() =
        this.Parse """
module Program =
    let internal fooBar = 0
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``camelCase should not be flagged because it is public``() =
        this.Parse """
module Program =
    let fooBar = 0
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``camelCase should not be flagged because it is private``() =
        this.Parse """
module Program =
    let private fooBar = 0
"""

        this.AssertNoWarnings()

