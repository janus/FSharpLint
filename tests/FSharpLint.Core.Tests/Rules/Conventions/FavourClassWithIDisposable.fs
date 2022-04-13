module FSharpLint.Core.Tests.Rules.Conventions.FavourClassWithIDisposable

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestBindingFavourClassWithIDisposable() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourClassWithIDisposable.rule)

    [<Test>]
    member this.NewWithIDisposalShouldNotProduceError() =
        this.Parse
            """
open System

let getDisposable () = { new IDisposable with 
                           member x.Dispose() = printfn "I am disposed" }

type SomeType () =
   let disposable = getDisposable ()
   interface IDisposable with
      member x.Dispose() = disposable.Dispose()

let f2 ()= 
   use someType = new SomeType()
   printfn "hello"
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.NewWithoutIDisposalShouldProduceError() =
        this.Parse
            """
type Example(locationY: int, locationX: int) =
    do printfn "y = %i and x = %i" locationY locationX

let foo = new Example(89, 100)
"""

        Assert.IsTrue(this.ErrorsExist)