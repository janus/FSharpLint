module FSharpLint.Core.Tests.Rules.Conventions.ParameterNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.CamelCase
      Underscores = Some NamingUnderscores.AllowPrefix
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsParameterNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ParameterNames.rule config)

    [<Test>]
    member this.FunctionParameterIsCamelCase() =
        this.Parse """
module Program
  let main dog = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ConstructorParameterIsPascalCase() =
        this.Parse """
module Program
  type MyClass2(Cats) as this =
    member this.PrintMessage() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 16))

    [<Test>]
    member this.ConstructorParameterIsCamelCase() =
        this.Parse """
module Program
  type MyClass2(cats) as this =
    member this.PrintMessage() = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.CompilerGeneratedArgumentName() =
        this.Parse """
module Program
(fun _ -> ())
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ParameterUnionCaseContainingValueDoesNotGenerateWarning() =
        this.Parse("""
module Program

type SingleCaseDU = SingleCaseDU of int
let extractInt (SingleCaseDU myInt) =
  myInt

let singleCaseDU = SingleCaseDU 5

let result = extractInt singleCaseDU""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Quick fix for underscores with config of `AllowPrefix` will only remove underscores not prefixing the identifier.``() =
        let source = """
module Program

let someFunction __foo_bar = 0
"""

        let expected = """
module Program

let someFunction __foobar = 0
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Quick fix for camel case takes into account underscore prefixes.``() =
        let source = """
module Program

let foo _X = 0
"""

        let expected = """
module Program

let foo _x = 0
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

let camelCaseConfig =
    { NamingConfig.Naming = Some NamingCase.CamelCase
      Underscores = None
      Prefix = None
      Suffix = None }

[<TestFixture>]
type TestConventionsCamelCase() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ParameterNames.rule camelCaseConfig)

    [<Test>]
    member this.``PascalCase should not be flagged because FooBar is not a parameter``() =
        this.Parse """
type SomeType() =
    let FooBar = 0
    member this.SomePublicMember() =
        ()
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``PascalCase should be flagged because Bar is parameter``() =
        this.Parse """
module SomeModule =
    let Foo Bar =
        0
"""

        Assert.IsTrue this.ErrorsExist
