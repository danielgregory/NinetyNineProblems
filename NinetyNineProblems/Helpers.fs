(* Helper functions go here! *)
module Utils.Helpers

// for use in sortBy:
// e.g. Seq.sortBy asc 
let asc =
    (fun e -> + e)

let desc =
    (fun e -> - e)

// examples
let lst = [1;2;3;4;5;6]
let ans = Seq.sortBy desc lst
let ans2 = Seq.sortBy asc lst