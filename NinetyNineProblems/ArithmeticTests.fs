﻿(* Tests for the the arithmetic problems. *)
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
        // some other examples
        Assert.IsTrue (isPrime 2)
        Assert.IsTrue (isPrime 5)
        Assert.IsTrue (isPrime 7)
        Assert.IsTrue (isPrime 769)

        Assert.IsFalse (isPrime 24)
   
    [<Test>]
    member this.TestGcd() =
        let a = 1071
        let b = 462
        Assert.AreEqual (21, gcd a b)

    // interestingly gcd2 is much faster than gcd.
    [<Test>]
    member this.TestGcd2() =
        let a = 1071
        let b = 462
        Assert.AreEqual (21, gcd2 a b)
        