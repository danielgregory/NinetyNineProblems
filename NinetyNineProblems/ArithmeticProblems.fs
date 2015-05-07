﻿(* Module contains solutions to problems 31 ... all to do with arithmetic *)
module Arithmetic

// 31. Determine whether a given integer number is prime.
let isPrime n =
    let integers = seq {for i in 2..n/2 -> i}
    let divisors = Seq.skipWhile(fun i -> n % i <> 0) integers
    if n = 0 then false else Seq.isEmpty divisors

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

// 35. Determine the prime factors of a given positive integer.
// e.g. primeFactors 315 -> [3; 3; 5; 7]
// using active patterns
let (|Prime|NotPrime|) n = if isPrime n then Prime n else NotPrime n

let findFirstPrime n = 
    let result = Seq.tryFind(fun i -> n % i = 0 && isPrime i) [2 .. n]
    match result with
    | Some result -> (result, n / result)
    | None -> failwith "not a value"
     
let primeFactors input =
    let rec innerPrimeFactors (n, m) acc =
        match n,m with
        | (Prime n, Prime m) -> n::m::acc
        | (NotPrime n, Prime m) -> innerPrimeFactors (findFirstPrime n) (m::acc) 
        | (Prime n, NotPrime m) -> innerPrimeFactors (findFirstPrime m) (n::acc)  
        | (NotPrime n, _) -> innerPrimeFactors (findFirstPrime n) acc
    let result = innerPrimeFactors (findFirstPrime input) []
    result |> Seq.sortBy(fun i -> i) 