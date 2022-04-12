module FSharpLint.Core.Tests.Rules.Conventions.FavourIDisposableWithNewKeyword

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestBindingFavourIDisposableWithNewKeyword() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourIDisposableWithNewKeyword.rule)

    [<Test>]
    member this.NewWithIDisposalShouldNotProduceError() =
        this.Parse
            """
open System

let getdisposable () = {new IDisposable with 
                           member x.Dispose() = printfn "I am disposed"}

type Atype () =
   let d = getdisposable ()
   interface IDisposable with
      member x.Dispose() = d.Dispose()

let f2 ()= 
   use a = new Atype()
   printfn "hello"
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.NewWithoutIDisposalShouldProduceError() =
        this.Parse
            """
type Example(y: int, x: int) =
    do Printfn "y = %i and x = %i" y x

let foo = new Example(89, 100)
"""

        Assert.IsTrue(this.ErrorsExist)