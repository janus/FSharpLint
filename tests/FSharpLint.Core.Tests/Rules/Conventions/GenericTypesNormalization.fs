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

    [<Test>]
    member this.``generic type style should be improved (3)``() =
        this.Parse "type 'a Foo = Foo of 'a"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``generic type style should be improved (4)``() =
        this.Parse """
//[<ApiExplorerSettings(IgnoreApi = true)>]
[<Route("api/v1/admin/import")>]
type RoleAdminImportController(akkaService: AkkaService) =
    inherit Controller()
    [<HttpGet("jobs/all");
      ProducesResponseType(typeof<bool>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Read)>]
    member _.ListJobs(): Task<UserCmdResponseMsg> =
        task {
            return!
                akkaService.ImporterSystem.ApiMaster <? ApiMasterMsg.GetAllJobsCmd
        }
    [<HttpPost("jobs/create");
      DisableRequestSizeLimit;
      RequestFormLimits(MultipartBodyLengthLimit = 509715200L);
      ProducesResponseType(typeof<RoleChangeSummaryDto list>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Write)>]
    member _.StartJob(file: IFormFile, [<FromQuery>] args: ImporterJobArgs) =
        let importer = akkaService.ImporterSystem
        ActionResult.ofAsyncResult <| asyncResult {
            let! state =
                (LowerCaseString.create args.State, file)
                |> pipeObjectThroughValidation [ (fst, [stateIsValid]); (snd, [(fun s -> Ok s)]) ]
            let! filePath = FormFile.downloadAsTemp file
            let job =
                { JobType = EsriBoundaryImport
                  FileToImport = filePath
                  State = state
                  DryRun = args.DryRun }
            importer.ApiMaster <! StartImportCmd job
            return Ok job
        }
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``recursive types in signature file``() =
        this.Parse """
type Cmd<'msg> = Cmd'<'msg> list
and private Cmd'<'msg> = Send<'msg> -> unit
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``generic type style should be in multiline recursive types``() =
        this.Parse """
type ViewBinding<'model,'msg> = string * Variable<'model,'msg>
and ViewBindings<'model,'msg> = ViewBinding<'model,'msg> list
and Variable<'model,'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model,'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model,'msg>
    | BindCmd of Execute<'model,'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model,'msg>
    | BindMap of Getter<'model> * (obj -> obj)
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``recursive classes``() =
        this.Parse """
type Folder(pathIn: string) =
    let path = pathIn
    let filenameArray : string array = System.IO.Directory.GetFiles(path)
    member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray
and File(filename: string, containingFolder: Folder) =
    member __.Name = filename
    member __.ContainingFolder = containingFolder
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``should keep type annotations on auto properties``() =
        this.Parse """
type Document(id : string, library : string, name : string) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
    member val LibraryID : string option = None with get, set
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``should keep type annotations in class``() =
        this.Parse """
type Document(id : string, library : string, name : string option) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``comment before multiline class member``() =
        this.Parse """
type MaybeBuilder () =
    member inline __.Bind
// meh
        (value, binder : 'T -> 'U option) : 'U option =
        Option.bind binder value
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``generic type style (3)``() =
        this.Parse "type Foo<'a> = Foo of 'a"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``generic type style (4)``() =
        this.Parse """
//[<ApiExplorerSettings(IgnoreApi = true)>]
[<Route("api/v1/admin/import")>]
type RoleAdminImportController(akkaService: AkkaService) =
    inherit Controller()
    [<HttpGet("jobs/all");
      ProducesResponseType(typeof<bool>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Read)>]
    member _.ListJobs(): Task<UserCmdResponseMsg> =
        task {
            return!
                akkaService.ImporterSystem.ApiMaster <? ApiMasterMsg.GetAllJobsCmd
        }
    [<HttpPost("jobs/create");
      DisableRequestSizeLimit;
      RequestFormLimits(MultipartBodyLengthLimit = 509715200L);
      ProducesResponseType(typeof<list<RoleChangeSummaryDto>>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Write)>]
    member _.StartJob(file: IFormFile, [<FromQuery>] args: ImporterJobArgs) =
        let importer = akkaService.ImporterSystem
        ActionResult.ofAsyncResult <| asyncResult {
            let! state =
                (LowerCaseString.create args.State, file)
                |> pipeObjectThroughValidation [ (fst, [stateIsValid]); (snd, [(fun s -> Ok s)]) ]
            let! filePath = FormFile.downloadAsTemp file
            let job =
                { JobType = EsriBoundaryImport
                  FileToImport = filePath
                  State = state
                  DryRun = args.DryRun }
            importer.ApiMaster <! StartImportCmd job
            return Ok job
        }
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``recursive types in signature file (1)``() =
        this.Parse """
type Cmd<'msg> = list<Cmd'<'msg>>
and private Cmd'<'msg> = Send<'msg> -> unit
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``generic type style should be in multiline recursive types (2)``() =
        this.Parse """
type ViewBinding<'model,'msg> = string * Variable<'model,'msg>
and ViewBindings<'model,'msg> = list<ViewBinding<'model,'msg>>
and Variable<'model,'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model,'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model,'msg>
    | BindCmd of Execute<'model,'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model,'msg>
    | BindMap of Getter<'model> * (obj -> obj)
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``recursive classes (1)``() =
        this.Parse """
type Folder(pathIn: string) =
    let path = pathIn
    let filenameArray : array<string> = System.IO.Directory.GetFiles(path)
    member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray
and File(filename: string, containingFolder: Folder) =
    member __.Name = filename
    member __.ContainingFolder = containingFolder
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``type annotations on auto properties (1)``() =
        this.Parse """
type Document(id : string, library : string, name : string) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
    member val LibraryID : option<string> = None with get, set
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``type annotations in class(1)``() =
        this.Parse """
type Document(id : string, library : string, name : option<string>) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``comment before multiline class member(1)``() =
        this.Parse """
type MaybeBuilder () =
    member inline __.Bind
// meh
        (value, binder : 'T -> option<'U>) : option<'U> =
        Option.bind binder value
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``quick fix for  generic type style should be improved (3)``() =
        let source = "type 'a Foo = Foo of 'a"

        let expected = "type Foo<'a> = Foo of 'a"

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for generic type style should be improved (4)``() =
        let source = """
//[<ApiExplorerSettings(IgnoreApi = true)>]
[<Route("api/v1/admin/import")>]
type RoleAdminImportController(akkaService: AkkaService) =
    inherit Controller()
    [<HttpGet("jobs/all");
      ProducesResponseType(typeof<bool>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Read)>]
    member _.ListJobs(): Task<UserCmdResponseMsg> =
        task {
            return!
                akkaService.ImporterSystem.ApiMaster <? ApiMasterMsg.GetAllJobsCmd
        }
    [<HttpPost("jobs/create");
      DisableRequestSizeLimit;
      RequestFormLimits(MultipartBodyLengthLimit = 509715200L);
      ProducesResponseType(typeof<RoleChangeSummaryDto list>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Write)>]
    member _.StartJob(file: IFormFile, [<FromQuery>] args: ImporterJobArgs) =
        let importer = akkaService.ImporterSystem
        ActionResult.ofAsyncResult <| asyncResult {
            let! state =
                (LowerCaseString.create args.State, file)
                |> pipeObjectThroughValidation [ (fst, [stateIsValid]); (snd, [(fun s -> Ok s)]) ]
            let! filePath = FormFile.downloadAsTemp file
            let job =
                { JobType = EsriBoundaryImport
                  FileToImport = filePath
                  State = state
                  DryRun = args.DryRun }
            importer.ApiMaster <! StartImportCmd job
            return Ok job
        }
"""

        let expected = """
//[<ApiExplorerSettings(IgnoreApi = true)>]
[<Route("api/v1/admin/import")>]
type RoleAdminImportController(akkaService: AkkaService) =
    inherit Controller()
    [<HttpGet("jobs/all");
      ProducesResponseType(typeof<bool>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Read)>]
    member _.ListJobs(): Task<UserCmdResponseMsg> =
        task {
            return!
                akkaService.ImporterSystem.ApiMaster <? ApiMasterMsg.GetAllJobsCmd
        }
    [<HttpPost("jobs/create");
      DisableRequestSizeLimit;
      RequestFormLimits(MultipartBodyLengthLimit = 509715200L);
      ProducesResponseType(typeof<list<RoleChangeSummaryDto>>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Write)>]
    member _.StartJob(file: IFormFile, [<FromQuery>] args: ImporterJobArgs) =
        let importer = akkaService.ImporterSystem
        ActionResult.ofAsyncResult <| asyncResult {
            let! state =
                (LowerCaseString.create args.State, file)
                |> pipeObjectThroughValidation [ (fst, [stateIsValid]); (snd, [(fun s -> Ok s)]) ]
            let! filePath = FormFile.downloadAsTemp file
            let job =
                { JobType = EsriBoundaryImport
                  FileToImport = filePath
                  State = state
                  DryRun = args.DryRun }
            importer.ApiMaster <! StartImportCmd job
            return Ok job
        }
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for recursive types in signature file``() =
        let source = """
type Cmd<'msg> = Cmd'<'msg> list
and private Cmd'<'msg> = Send<'msg> -> unit
"""

        let expected = """
type Cmd<'msg> = list<Cmd'<'msg>>
and private Cmd'<'msg> = Send<'msg> -> unit
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for generic type style should be in multiline recursive types``() =
        let source = """
type ViewBinding<'model,'msg> = string * Variable<'model,'msg>
and ViewBindings<'model,'msg> = ViewBinding<'model,'msg> list
and Variable<'model,'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model,'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model,'msg>
    | BindCmd of Execute<'model,'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model,'msg>
    | BindMap of Getter<'model> * (obj -> obj)
"""

        let expected = """
type ViewBinding<'model,'msg> = string * Variable<'model,'msg>
and ViewBindings<'model,'msg> = list<ViewBinding<'model,'msg>>
and Variable<'model,'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model,'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model,'msg>
    | BindCmd of Execute<'model,'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model,'msg>
    | BindMap of Getter<'model> * (obj -> obj)
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for recursive classes``() =
        let source = """
type Folder(pathIn: string) =
    let path = pathIn
    let filenameArray : string array = System.IO.Directory.GetFiles(path)
    member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray
and File(filename: string, containingFolder: Folder) =
    member __.Name = filename
    member __.ContainingFolder = containingFolder
"""

        let expected = """
type Folder(pathIn: string) =
    let path = pathIn
    let filenameArray : array<string> = System.IO.Directory.GetFiles(path)
    member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray
and File(filename: string, containingFolder: Folder) =
    member __.Name = filename
    member __.ContainingFolder = containingFolder
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for type annotations on auto properties``() =
        let source = """
type Document(id : string, library : string, name : string) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
    member val LibraryID : string option = None with get, set
"""

        let expected = """
type Document(id : string, library : string, name : string) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
    member val LibraryID : option<string> = None with get, set
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for type annotations in class``() =
        let source = """
type Document(id : string, library : string, name : string option) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
"""

        let expected = """
type Document(id : string, library : string, name : option<string>) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``quick fix for comment before multiline class member``() =
        let source = """
type MaybeBuilder () =
    member inline __.Bind
// meh
        (value, binder : 'T -> option<'U>) : 'U option =
        Option.bind binder value
"""

        let expected = """
type MaybeBuilder () =
    member inline __.Bind
// meh
        (value, binder : 'T -> option<'U>) : option<'U> =
        Option.bind binder value
"""
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)
