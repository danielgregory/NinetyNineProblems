(* Module contains solutions to problems 1 to 10 *)
module ListProcessingProblems

open System
open Utils.Helpers
    
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
    | _ -> failwith "Something has gone wrong!"

// 2. Find the last but one element of a list.
// e.g. GetLastButOne [1;2;3;4] -> 3
let rec GetLastButOne lst = 
    match lst with 
    | [x; y] -> x
    | x :: xs -> GetLastButOne xs
    | _ -> failwith "Oh no! It's gone wrong!"

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

// using fold
let foldReverse lst =
    List.fold (fun acc e -> e::acc) [] lst

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
// e.g. GroupAndCount ['a'; 'b'; 'b'; 'c'; 'c'; 'c';] -> [(1, 'a'); (2, 'b'); (3, 'c')]
let GroupAndCount lst =
    [ for e in (Partition lst) -> ((List.length e), (List.head e)) ]

// 11. This problem is a little hard to do with the lists in F#.
// e.g. GroupAndCount ['a'; 'b'; 'b'; 'c'; 'c'; 'c';] -> ['a'; (2, 'b'); (3, 'c')]

// 12 Reverse of problem 10.
// DeGroup [(1, 'a'); (2, 'b'); (3, 'c')]

// first, take a tuple and return a single list
let Expand tpl = 
    let count, item = tpl
    List.init count (fun _ -> item)

let DeGroup lst =
    List.fold (fun acc tpl -> acc@(Expand tpl)) [] lst

// 13. no idea what this problem means

// 14. Duplicate the elements of a list.
// e.g. Duplicate ['a'; 'b'; 'c'; 'c'; 'd'] -> ['a'; 'a'; 'b'; 'b'; 'c'; 'c'; 'c'; 'c'; 'd'; 'd']
let Duplicate lst =
    let grouped = GroupAndCount lst
    let doubledIt = [for count,elem in grouped -> (count * 2, elem)]
    DeGroup doubledIt

// 15. DuplicateByN(3, List('a, 'b, 'c, 'c, 'd))

let DuplicateByN n lst = 
    let grouped = GroupAndCount lst
    // new list of tuples with the count scaled by a factor of n
    let scaledByN = [for count,elem in grouped -> (count * n, elem)]
    DeGroup scaledByN
    
// 16. Drop every Nth element from a list.
let DropNth n list =
    List.mapi(fun i e -> (i+1,e)) list
    |> List.filter (fun (i,e) -> i%n<>0)
    |> List.map snd

// 17. Split a list into two parts.
// e.g. Split 4 [1;2;3;4;5;6;7;8] ->  [[4; 3; 2; 1]; [5; 6; 7; 8]]
// (I guess it could be problem that both lists are reversed, but I'm choosing not to mind.)
let Split n lst =
    let rec InnerSplit acc lst = 
        match lst with
        | x :: xs when List.length acc < n -> InnerSplit (x::acc) xs
        | _ -> [acc; lst]   
    InnerSplit [] lst

// 18.  Extract a slice from a list.
// Not sure this suits F# well since we think of lists as a head plus a tail.
// You do this sort of thing in Python a lot though.

// ok solution but it involves iterating over the sequence three times.
let Slice (a:int) (b:int) lst = 
    let lst = List.mapi (fun i e -> (i,e)) lst
    List.filter (fun (e,i) -> i>= a && i < b) lst
    |> List.map snd

let ArraySlice (a:int) (b:int) arr = 
    let rec slice index result =
        if index < b then
            let e = Array.get arr index
            slice (index + 1) (e::result)
        else
            result
    slice a List.empty

// 19 Rotate a list to the left
let rec innerReverse n lst = 
        match lst with
        | x :: xs when n > 0 -> innerReverse (n - 1) (xs@[x])
        | _  -> lst

// question wants this function
let rotateLeft n lst =    
    innerReverse n lst

// interestingly we get the rotate right like this:
let rotateRight n lst = 
    lst 
    |> List.rev 
    |> innerReverse n 
    |> List.rev 
    
// 20 Remove the Kth element from a list.
// F# lists aren't really meant for this kind of thing
// it's only easy to remove the head of the list.
// so in this example we keep rotating the list and chopping
// off the head of the list
let remove k lst =
    let lst2 = lst |> rotateLeft k |> List.tail
    let len = List.length lst
    rotateLeft (len - k - 1) lst2

//21 Insert an element at a given position
let insertAt k n lst =  
    rotateRight k (n::(rotateLeft k lst))

// 22 range(4, 9)
// List(4, 5, 6, 7, 8, 9) 
let range a b =
    [for i in a .. b -> i]

// using recursion
let rec recRange acc seed finish =
    if seed <= finish then
        recRange (seed::acc) (seed + 1) finish
    else 
        acc |> List.rev

// example using infinite seqs
let range2 start finish =
    let s = Seq.initInfinite(fun i -> i + start) 
    s |> Seq.take (finish - 1)  
      |> Seq.toList

// 23 Extract a given number of randomly selected elements from a list.
let extractRnd n lst = 
    let r = new Random()
    seq {for i in 1..n -> let num = r.Next((List.length lst) - 1)
                          List.nth lst num } |> Seq.toList

// 24 Lotto: Draw N different random numbers from the set 1..M.
let lotto n m =
    extractRnd n [1..m]

// 25 Generate a random permutation of the elements of a list.
// e.g. permute [a,b,c,d,e,f] --> [b,a,d,c,e,f]
// strategy is to generate all permutations of a given list and then
// randomly pick one.
let alphabetlst = ["a"; "b"; "c"; "d"; "e"; "f"]

let rec getRndPerm (lst: string list) =
    let arr = List.toArray lst
    let size = Array.length arr

    let r = new System.Random()
    let randoms = 
        Seq.initInfinite(fun _ -> r.Next(0, size))
        |> Seq.distinct
        |> Seq.take size
        |> Seq.toList

    List.map(fun e -> arr.[e]) randoms

// seems to me that the above problem is really better suited
// to arrays rather than lists as we have indexed access on 
// arrays...
// TODO: allow for generic list input of any type.

// 26 Generate the combinations of K distinct objects chosen from the N elements of a list
// read 'choose n from k':
//let choose n k =
//    // get the total number of combinations
//    let count = (factorial n) / (factorial k)
//    let rec combinations n k acc =
//        while (List.length acc) < count do
//     
//     combinations n k []