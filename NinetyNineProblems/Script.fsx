// NINETY-NINE PROBLEMS

// I've taken the problems from here:
// http://aperiodic.net/phil/scala/s-99/
// (where solutions can be obtained in Scala ... though I didn't cheat!)

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

// 6. Find out whether a list is a palindrome.
// e.g. IsPalidrome [1;2;3;3;2;1] -> true
// IsPalidrome only makes sense for lists with an even
// number of elements.

// solution 1 ... not sure this is the most elegant!
let IsPalindrome lst =
    if (List.length lst % 2 = 0) then
        let FirstHalf = lst |> Seq.take (List.length lst / 2) |> Seq.toList 
        let SecondHalf = List.rev FirstHalf
        let Palidrome = FirstHalf @ SecondHalf
        // test for equality
        Palidrome = lst
    else
        false

// solution 2 .... obviously just take the mirror image!
let IsMirror lst = 
        let reversed = List.rev lst
        reversed = lst

// 7. Flatten a nested list structure.
// e.g. Flatten [1; [2;3;4]; 5; 6]
// F# lists must contain only one type, so we create a discriminated union
type ListOrInt = 
    | I of int
    | L of ListOrInt list

//e.g:
let x = L([I(1); L([I(2);I(3);L([I(88); I(27)])]); I(5); I(6)])

// ok solution, uses recursion.
// works by converting all integers in the lst into single element lists.
// so that for example 3 -> [3] 
let rec Flatten lst =
        match lst with
        | I x -> [x]
        | L x -> List.concat(List.map Flatten x)

// using tail recursion ... I think. TODO: is this tail recursion!? ans ... no
// It is slower than Flatten.
let TailFlatten lst =
    let rec RecFlatten acc lst =
        match lst with
        | I x -> x :: acc
        | L x -> List.concat(List.map (RecFlatten acc) x)
    RecFlatten List.Empty lst

// another possible solution:
let rec FlattenSeq lst =
    seq {for x in lst do 
            yield x
            yield! FlattenSeq x}

// e.g. 
let s = [[[1];[2];[3]]; [[4];[5]]; [[5]]]


// using folds within folds ...
let rec FoldFlatten acc lst = 
    let ans = List.fold(fun acc e -> match e with
                                     | I a -> (I a)::acc
                                     | L a -> FoldFlatten acc a) acc lst
    ans |> List.rev

// flatten with seq expression
let t:ListOrInt list =   [I 1;I 2;I 3; L [I 4;I 5;L [I 88; I 99; I 100]]]
let rec SeqFlatten list =  
    let FlatSeq = 
        seq {for i in list do
                            match i with
                            | I n -> yield (I n)
                            | L lst -> yield! SeqFlatten lst
           }
    FlatSeq |> List.ofSeq

// interlude: here we work out whether there is a performance difference between Flatten and TailFlatten.
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

    // side effects
    System.Console.WriteLine(elapsed1)
    System.Console.WriteLine(elapsed2)

    let diff = elapsed1.CompareTo(elapsed2)
    match diff with
    | x when x > 0 -> "f2 is faster than f1"
    | x when x < 0 -> "f1 is faster than f2"
    | x when x = 0 -> "same"
    | _ -> failwith("gone wrong")

// Example output at the F# Interactive:
// > Compare Flatten TailFlatten x;;
// 5476
// 6577
// val it : string = "f1 is faster than f2"
// Seems to indicate that Flatten is faster than the tail recursion version ...

// back to the problems
// 8. Eliminate consecutive duplicates in list of elements.
// e.g. [1;1;2;3;4;4;6] --> [1;2;3;4;6]
let lst = [1;1;2;3;4;4;6]

// this works but seems a little complicated.
// the resulting list is returned in reverse order to we have to call List.rev on it
// which seems inefficient. TODO: is there a better way?
let RemoveDuplicates (lst:int list) = 
    let rec Compress acc lst = 
        match lst with
        | [x] -> x::acc
        | x :: xs -> 
            let h = (List.head xs)
            if h = x then
                let acc = acc
                Compress acc xs 
            else
                let acc = x :: acc  
                Compress acc xs
         | [] ->  failwith("empty list!")   
    Compress [] lst 
    |> List.rev

// 9. Pack consecutive duplicates of list elements into sublists.
// 'partition' function : partition [1;1;2;3;4;4;6] -> [[1;1];[2];[3];[4;4];[6]]
let IsEmpty lst =
    (List.length lst) = 0

let IsNotEmpty lst =
    (List.length lst) <> 0

let rec Partition lst =    
    let rec InnerPartition acc1 acc2 lst = 
        match lst with
        | x :: xs when IsEmpty acc1 -> InnerPartition [x] acc2 xs
        | x :: xs when IsNotEmpty acc1 && List.head acc1 = x -> InnerPartition (x::acc1) acc2 xs
        | x :: xs when IsNotEmpty acc1 && List.head acc1 <> x -> InnerPartition [x] (acc1::acc2) xs
        | _ -> acc1::acc2
    (InnerPartition [] [] lst) |> List.rev

// 10. Run-length encoding of a list.
// e.g. Encode ['a'; 'b'; 'b'; 'c'; 'c'; 'c';] -> [(1, 'a'); (2, 'b'); (3, 'c')]
let GroupAndCount lst =
    [ for e in (Partition lst) -> ((List.length e), (List.head e)) ] 