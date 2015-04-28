(* Module contains the tests for the solutions to problems 1 to 10. *)
module Tests1

open ListProcessingProblems
open NUnit.Framework

[<TestFixture>]
type TestListProblems() =
    
    [<Test>]
    member this.TestGetLast() =
        let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]
        let actual = GetLast lst
        Assert.AreEqual(67, actual)

    [<Test>]
    member this.TestGetLastWithStringyList() =
        let lst = ["To strive"; "to seek"; "to find"; "and"; "not to yield"]
        let actual = GetLast lst 
        Assert.AreEqual("not to yield", actual)

    [<Test>]
    member this.TestGetLast2() =
        let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]
        let actual = GetLast2 lst
        Assert.AreEqual(67, actual)

    [<Test>]
    member this.TestGetLast2Stringy() =
        let lst = ["To strive"; "to seek"; "to find"; "and"; "not to yield"]
        let actual = GetLast2 lst 
        Assert.AreEqual("not to yield", actual)

    [<Test>]
    member this.TestGetLastButOne() =
         let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]
         let actual = GetLastButOne lst
         Assert.AreEqual(1, actual)

    [<Test>]
    member this.TestGetNth() =
        let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]
        let actual = GetNth 3 lst
        Assert.AreEqual(4, actual)

    [<Test>]
    member this.TestGetNthByFiltering() =
        let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]
        let actual = GetNthByFiltering 3 lst
        Assert.AreEqual(4, actual)

    // TODO
     // confirm that the recursive version of GetNth is faster
     // than the filter version
     (* [<Test>]
     member this.CompareGetNths() =
        let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]

        // partially apply functions
        let recursiveActual = GetNth 10
        let filteredActual = GetNth 10

        Compare recursiveActual filteredActual 10 *)

    [<Test>]
    member this.TestCount() =
        let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]
        let actual = Count lst
        Assert.AreEqual(15, actual)

    [<Test>]
    member this.TestReverse() =
        // input list
        let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]
        
        let expected = [67; 1; 61; 4; 3; 0; 9; 8; 7; 6; 5; 4; 3; 2; 1]
        let actual = Reverse lst

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.TestTailReverse() =
        // input list
        let lst = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 3; 4; 61; 1; 67]
        
        let expected = [67; 1; 61; 4; 3; 0; 9; 8; 7; 6; 5; 4; 3; 2; 1]
        let actual = TailReverse lst

        Assert.AreEqual(expected, actual) 
     
     [<Test>]
     member this.TestIsPalindromeWhereFalse() =
        let lst = [1;2;3;4;5;6]
        Assert.IsFalse(IsPalindrome lst)  

     [<Test>]
     member this.TestIsPalindromeWhereTrue() =
        let lst = [1;2;3;3;2;1]
        Assert.IsTrue(IsPalindrome lst) 

    [<Test>]
    member this.TestIsMirrorWhereFalse() =
        let lst = [1;2;3;4;5;6]
        Assert.IsFalse(IsMirror lst)
        
    [<Test>]
     member this.TestIsMirrorTrue() =
        let lst = [1;2;3;3;2;1]
        Assert.IsTrue(IsMirror lst)   

    // TODO look again at all these flatten functions.
    [<Test>]
     member this.TestFlatten() =
        let x = L([I(1); L([I(2);I(3);L([I(88); I(27)])]); I(5); I(6)])
        let actual = Flatten x
        Assert.AreEqual([1;2;3;88;27;5;6], actual) 
     
     [<Test>]
     member this.TestTailFlatten() =
        let x = L([I(1); L([I(2);I(3);L([I(88); I(27)])]); I(5); I(6)])
        let actual = TailFlatten x
        Assert.AreEqual([1;2;3;88;27;5;6], actual)
     
     [<Test>]
     member this.TestFoldFlatten() =
        let x = [I(1); L([I(2);I(3);L([I(88); I(27)])]); I(5); I(6)]
        let actual = FoldFlatten [] x
        Assert.AreEqual([I 1; I 2; I 3; I 88; I 27; I 5; I 6], actual)      
     
       
     [<Test>]
     member this.TestSeqFlatten() =
        let x =  [I 1;I 2;I 3; L [I 4;I 5;L [I 88; I 99; I 100]]]
        let actual = SeqFlatten x
        Assert.AreEqual([I 1; I 2; I 3; I 4; I 5; I 88; I 99; I 100], actual)
     
     
     // remove consecutive duplicates ...    
     [<Test>]
     member this.TestRemoveDuplicates() =
        let input = [1;2;3;3;3;4;5;5;6;7]
        let expected = [1;2;3;4;5;6;7]
        let result = RemoveDuplicates input      
        Assert.AreEqual(expected, result)
        
     member this.TestPartition() =
        let input = [1;1;1;2;2;3;3;3;3;4;6;8;8]
        let expected = [[1; 1; 1]; [2; 2]; [3; 3; 3; 3]; [4]; [6]; [8; 8]]
        let result = Partition input
        Assert.AreEqual(expected, result)   
    



