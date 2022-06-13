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
        this.Parse "type Foo<'T> = Bar of 'T list"

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

    [<Test>]
    member this.``generic type style is used (1)``() =
        this.Parse "type Foo<'T> = Bar of list<'T>"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``generic type style is used (2)``() =
        this.Parse "type Foo<'T when 'T :> IDisposable> = { Bar: 'T }"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``generic type style is used in place of (string option * Node)``() =
        this.Parse """
type Node =
    { Name: string;
      NextNodes: list<(option<string> * Node)> }
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``generic type style is used instead of string list type signature in function parameter``() =
        this.Parse """
let MSBuildWithProjectProperties outputPath (targets: list<string>) =
    doingSomeStuff()
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``generic type style is used instead of the (string * string) list type signature in functions``() =
        this.Parse """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: string -> list<(string * string)>) projects =
    doingSomeStuff()
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``generic type style is used instead of the (string * string) list type signature in records``() =
        this.Parse """
type MSBuildParams =
    { Targets: list<string>
      Properties: list<(string * string)>
      MaxCpuCount: option<option<int>>
      ToolsVersion: option<string>
      Verbosity: option<MSBuildVerbosity>
      FileLoggers: option<list<MSBuildFileLoggerConfig>> }
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``generic type style is used instead of string * string * (string option) type signature in unions``() =
        this.Parse """
type DGML =
    | Node of string
    | Link of string * string * option<string>
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``quick fix for generic type style (1)``() =
        let source = "type 'T Foo when 'T :> IDisposable = { Bar: 'T }"
        let expected = "type Foo<'T when 'T :> IDisposable> = { Bar: 'T }"

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix generic type style should be improved (2)``() =
        let source =  "type Foo<'T> = Bar of 'T list"
        let expected = "type Foo<'T> = Bar of list<'T>"

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for string * string list type signature in functions``() =
        let source = """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: (string -> string) * string list) projects = doingSomeStuff()
"""

        let expected = """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: (string -> string) * list<string>) projects = doingSomeStuff()
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for string * string list type signature in unions``() =
        let source = """
type DGML =
    | Node of string
    | Link of string * string * (string option)
"""

        let expected = """
type DGML =
    | Node of string
    | Link of string * string * (option<string>)
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for (string option * Node) list type signature``() =
        let source =  """
type Node =
    { Name: string;
      NextNodes: (string option * Node) list }
"""

        let expected =  """
type Node =
    { Name: string;
      NextNodes: list<(option<string> * Node)> }
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for (string * string) list type signature in records``() =
        let source = """
type MSBuildParams =
    { Properties: list<(string * string)>
      MaxCpuCount: int option option
      ToolsVersion: string option
      Verbosity: MSBuildVerbosity option
      FileLoggers: MSBuildFileLoggerConfig list option }
"""

        let expected = """
type MSBuildParams =
    { Properties: list<(string * string)>
      MaxCpuCount: option<option<int>>
      ToolsVersion: string option
      Verbosity: MSBuildVerbosity option
      FileLoggers: MSBuildFileLoggerConfig list option }
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for (string * string) list type signature in functions``() =
        let source = """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: string -> (string * string) list) projects =
    doingSomeStuff()
"""

        let expected = """
let MSBuildWithProjectProperties outputPath (targets: string) (properties: string -> list<(string * string)>) projects =
    doingSomeStuff()
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for string list type signature in function parameter``() =
        let source = """
let MSBuildWithProjectProperties outputPath (targets: string list) =
    doingSomeStuff()
"""

        let expected = """
let MSBuildWithProjectProperties outputPath (targets: list<string>) =
    doingSomeStuff()
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``no quick fix needed (1)``() =
        let source = """
type MSBuildParams =
    { Targets: list<string>
      Properties: list<(string * string)>
      MaxCpuCount: option<option<int>>
      ToolsVersion: option<string>
      Verbosity: option<MSBuildVerbosity>
      FileLoggers: option<list<MSBuildFileLoggerConfig>> }
"""

        let expected = """
type MSBuildParams =
    { Targets: list<string>
      Properties: list<(string * string)>
      MaxCpuCount: option<option<int>>
      ToolsVersion: option<string>
      Verbosity: option<MSBuildVerbosity>
      FileLoggers: option<list<MSBuildFileLoggerConfig>> }
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``no quick fix needed (2)``() =
        let source = """
let MSBuildWithProjectProperties outputPath (targets: array<string>) =
    doingSomeStuff()
"""

        let expected = """
let MSBuildWithProjectProperties outputPath (targets: array<string>) =
    doingSomeStuff()
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)
