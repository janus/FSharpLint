module FSharpLint.Core.Tests.Rules.Conventions.GenericTypesNormalization

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsGenericTypesNormalization() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(GenericTypesNormalization.rule)

    [<Test>]
    member this.``generic type style should be improved (1)``() =
        this.Parse "type 'T Foo when 'T :> IDisposable = { Bar: 'T }"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``generic type style should be improved (2)``() =
        this.Parse "type Foo<'T> = Bar of 'T option"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``should understand the string * string list type signature in functions``() =
        this.Parse """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: (string -> string) * string list) projects = doingSomeStuff()
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``should understand the string * string list type signature in unions``() =
        this.Parse """
type DGML =
    | Node of string
    | Link of string * string * (string option)
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``should understand the (string option * Node) list type signature``() =
        this.Parse """
type Node =
    { Name: string;
      NextNodes: (string option * Node) list }
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``should understand the (string * string) list type signature in records``() =
        this.Parse """
type MSBuildParams =
    { Targets: string list
      Properties: (string * string) list
      MaxCpuCount: int option option
      ToolsVersion: string option
      Verbosity: MSBuildVerbosity option
      FileLoggers: MSBuildFileLoggerConfig list option }
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``should understand the (string * string) list type signature in functions``() =
        this.Parse """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: string -> (string * string) list) projects =
    doingSomeStuff()
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``should understand the string list type signature in function parameter``() =
        this.Parse """
let MSBuildWithProjectProperties outputPath (targets: string list) =
    doingSomeStuff()
"""

        Assert.IsTrue this.ErrorsExist

