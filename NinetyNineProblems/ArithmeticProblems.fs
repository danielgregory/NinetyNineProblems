(* Module contains solutions to problems 31 ... all to do with arithmetic *)
module Arithmetic

open System
// 31. Determine whether a given integer number is prime.
let isPrime n =
    let integers = seq {for i in 2..n/2 -> i}
    let divisors = Seq.skipWhile(fun i -> n % i <> 0) integers
    if n = 0 then false else if n = 2 then true 
    else Seq.isEmpty divisors
