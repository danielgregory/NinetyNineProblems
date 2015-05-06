(* Module contains solutions to problems 31 ... all to do with arithmetic *)
module Arithmetic

open System
// 31. Determine whether a given integer number is prime.
let isPrime n =
    let integers = seq {for i in 2..n/2 -> i}
    let divisors = Seq.skipWhile(fun i -> n % i <> 0) integers
    if n = 0 then false else if n = 2 then true 
    else Seq.isEmpty divisors

// 32 Find the greatest common divisor of two numbers using Euclid's algorithm.
let rec gcd a b = 
    let r = a % b
    if r = 0 then b else gcd b r

// TODO: why is this much faster than gcd?
let gcd2 a b = 
    let rec innergcd a b r = 
        if r = 0 then b else innergcd b r (b % r)
    innergcd a b (a % b)

// 33. Determine whether two positive integer numbers are coprime.
// Two numbers are coprime if their greatest common divisor equals 1.
let isCoPrime a b = 
    (gcd2 a b) = 1

// 34. Euler's so-called totient function phi(m) is defined as the number
//  of positive integers r (1 <= r <= m) that are coprime to m.
let phi m =
    let s = seq {for i in 1.. (m - 1) do if isCoPrime i m then yield i}
    Seq.length s