(* Module contains solutions to problems 31 ... all to do with arithmetic *)
module Arithmetic

open Utils.Helpers 

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

let isPrimeDivisor acc e =
    let input = 315
    if input % e = 0 && isPrime e then
        List.Cons(e, acc)
    else 
        acc

 // not quite a solution but I'm leaving this here as an example
 // of fold
let NotQuiteTheFullListOfprimeDivisors input =
    let testNumbers = [for x in 2 .. input / 2 -> x]
    List.fold (fun acc e -> isPrimeDivisor acc e) [] testNumbers

// this works when combined with outerPrimes
let rec primeDivisors acc numbers n =
    match numbers with
    | x :: xs when n % x = 0
        -> match x with
            | Prime x -> primeDivisors (x::acc) xs n
            | NotPrime x -> primeDivisors acc [for i in 2.. x / 2 -> i] x 
    | x :: xs when n % x <> 0 -> primeDivisors acc xs n
    | _ -> acc

let outerPrimes n =
    let numbers = [for x in 2 .. n / 2 -> x]
    let divisors = (primeDivisors [] numbers n)
    if Seq.length divisors = 1
    then Seq.append divisors divisors
    else divisors |> Seq.sortBy asc

// another solution, this time using fold
let rec lazyUniqueDivisors n =
    seq {for i in 2 .. n / 2 do 
            if n % i = 0 && isPrime i 
                then yield i
            else if n % i = 0 && not (isPrime i) 
                then yield! lazyUniqueDivisors i}

// returns product of all the elements in the lseq.
let listProduct s =
    Seq.fold (fun acc e -> acc * e)  1 s

let findMeThePrimeDivisors n =
    let divisors = (lazyUniqueDivisors n)
    let primes = Seq.fold (fun acc e -> 
                            if (listProduct acc) = n then
                                acc
                            else
                                 e::acc) [] divisors 
    // fix for square numbers - e.g. 9 = 3 x 3 
    if Seq.length primes = 1 
    then 
        Seq.append primes primes 
    else
        (primes |> Seq.sortBy asc)
           