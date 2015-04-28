(* Tests for the the arithmetic problems. *)
module ArithmeticTests

open Arithmetic
open NUnit.Framework

[<TestFixture>]
type TestArithmeticProblems() =
   
    [<Test>]
    member this.TestIsPrime() =
        let prime = 1319
        let notPrime = 25
        Assert.IsTrue (isPrime prime)
        Assert.IsFalse (isPrime notPrime)
