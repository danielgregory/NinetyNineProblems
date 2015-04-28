(* Module for timer function *)
module Utils.Timer

open System

let time f arg =
    let stopwatch = new System.Diagnostics.Stopwatch()
    stopwatch.Start()
    
    // do work
    List.map (fun _ -> f arg) [1..1000000] |> ignore    
    
    stopwatch.Stop()
    stopwatch.Elapsed

// function to allow us to compare two functions
let Compare f1 f2 arg = 
    let elapsed1 = time f1 arg
    let elapsed2 = time f2 arg

    Console.WriteLine(elapsed1)
    Console.WriteLine(elapsed2)

    let diff = elapsed1.CompareTo(elapsed2)
    match diff with
    | x when x > 0 -> "f2 is faster than f1"
    | x when x < 0 -> "f1 is faster than f2"
    | x when x = 0 -> "same"
    | _ -> failwith "gone wrong"