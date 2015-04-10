// NINETY-NINE PROBLEMS

// I've taken the problems from here:
// http://aperiodic.net/phil/scala/s-99/
// (where solutions can be obtained in Scala ... though I didn't cheat!)

// TODO: timer function
// TODO unit tests

// 1. find the last element of a list
// e.g. last [1;2;3;4] -> 4
let LastElement = Seq.last [1;2;3;4]

let rec GetLast lst = 
    if List.length lst = 0 
        then failwith("List length too small") 
    elif List.length lst = 1 
        then List.head lst
    else 
        GetLast (List.tail lst)
        
// this is a better/more functional solution
let rec GetLast2 lst = 
    match lst with
    | [x] -> x
    | x :: xs -> GetLast2 xs
    | _ -> failwith("Something has gone wrong!")

 // 2. Find the last but one element of a list.
 // e.g. GetLastButOne [1;2;3;4] -> 3
let rec GetLastButOne lst = 
    match lst with 
    | [x; y] -> x
    | x :: xs -> GetLastButOne xs
    | _ -> failwith("Oh no! It's gone wrong!")

// 3. Find the Nth element of a list. 
// (I'm taking the first element of the list to be the zeroth element)
// e.g. GetNth 2 [6;4;8;9] -> 8

// recursive version using match expression
let rec GetNth (n:int) lst = 
    match lst with 
    | x :: xs when n = 0 -> x
    | _ :: xs when n > 0 -> GetNth (n - 1) xs
    | _ -> failwith "Gone wrong!" 

// alternative version using filtering
let GetNthByFiltering n lst =
    let TupledList = List.mapi (fun i e -> (i, e)) lst
    TupledList 
    |> List.filter(fun (i, e) -> i = n)
    |> List.head
    |> snd

// 4.  Find the number of elements of a list
// Count ['a'; 'b'; 'c'] -> 3
// obviously we can use List.length

let Count lst = List.fold (fun acc e -> acc + 1) 0 lst 

// interlude:
// [1] @ [2;3;4;5] -> [1;2;3;4;5] is equivalent to List.concat
// 1::[2;3;4;5] -> [1;2;3;4;5]  is equivalent to List.Cons

// 5. Reverse a list. 
// e.g. Reverse [1;2;3;4;5] -> [5;4;3;2;1]
// first using recursion.
let rec Reverse lst  = 
    match lst with
    | x :: xs -> (Reverse xs) @ [x]
    | x -> x
 
// tail recursive version.
let TailReverse lst  =
    let rec Reverse acc lst =
        match lst with
        | x :: xs -> Reverse (x::acc) xs  
        | x -> acc
    Reverse List.Empty lst 




    