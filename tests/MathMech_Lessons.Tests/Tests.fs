namespace MathMech_lessons.Tests

open Expecto

module MyList =
    open MyListHW

    [<Tests>]
    let tests =
        testList "samples" [
            testCase "1 test for concatenation ML"
            <| fun _ ->
                let actualResult =
                    MyListHW.сoncatenation (Cons(1, Cons(5, Empty))) (Cons(2, Cons(9, Empty)))

                Expect.equal
                    actualResult
                    (Cons(1, Cons(5, (Cons(2, Cons(9, Empty))))))
                    "The result should be (Cons(1, Cons(5, (Cons(2, Cons(9, Empty))))))"

            testCase "2 test for concatenation ML"
            <| fun _ ->
                let actualResult = MyListHW.сoncatenation Empty Empty
                Expect.equal actualResult Empty "The result should be Empty"

            testCase "3 test for concatenation ML"
            <| fun _ ->
                let actualResult = MyListHW.сoncatenation (Cons(1, Cons(2, Cons(3, Empty)))) Empty

                Expect.equal
                    actualResult
                    (Cons(1, Cons(2, Cons(3, Empty))))
                    "The result should be Cons(1, Cons(2, Cons(3, Empty)))"

            testCase "1 test for BubbleSort ML"
            <| fun _ ->
                let actualResult =
                    MyListHW.bubbleSort(Cons(0, Cons(20, Cons(15, Cons(-100, Empty)))))

                Expect.equal
                    actualResult
                    (Cons(-100, Cons(0, Cons(15, Cons(20, Empty)))))
                    "The result should be Cons(-100, Cons(0, Cons(15, Cons(20, Empty))))"

            testCase "2 test for BubbleSort ML"
            <| fun _ ->
                let actualResult = MyListHW.bubbleSort Empty
                Expect.equal actualResult Empty "The result should be Empty"

            testCase "3 test for bubbleSort ML"
            <| fun _ ->
                let actualResult = MyListHW.bubbleSort(Cons("ab", Cons("a", Cons("abc", Empty))))

                Expect.equal
                    actualResult
                    (Cons("a", Cons("ab", Cons("abc", Empty))))
                    "The result should be (Cons('a', Cons('ab', Cons('abc', Empty))))"

            testCase "4 test for BubbleSort ML"
            <| fun _ ->
                let actualResult = MyListHW.bubbleSort(Cons(5, Cons(5, Cons(5, Empty))))

                Expect.equal
                    actualResult
                    (Cons(5, Cons(5, Cons(5, Empty))))
                    "The result should be Cons(5, Cons(5, Cons(5, Empty )))"

            testCase "5 test for BubbleSort ML"
            <| fun _ ->
                let actualResult = MyListHW.bubbleSort(Cons(0, Empty))
                Expect.equal actualResult (Cons(0, Empty)) "The result should be Cons(0, Empty)"

            testCase "1 test for QuickSort ML"
            <| fun _ ->
                let actualResult = MyListHW.quickSort(Cons(0, Empty))
                Expect.equal actualResult (Cons(0, Empty)) "The result should be Cons(0, Empty)"

            testCase "2 test for QuickSort ML"
            <| fun _ ->
                let actualResult = MyListHW.quickSort(Cons(1, Cons(5, Cons(-10, Empty))))

                Expect.equal
                    actualResult
                    (Cons(-10, Cons(1, Cons(5, Empty))))
                    "The result should be Cons(-10, Cons(1, Cons(5, Empty)))"

            testCase "3 test for QuickSort ML"
            <| fun _ ->
                let actualResult = MyListHW.quickSort Empty
                Expect.equal actualResult Empty "The result should be Empty"
        ]

module OOPList =
    open OOPListHW
    //open MyListHW
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "1 test for concatenation OL"
            <| fun _ ->
                let actualResult =
                    let lst1 = EmptyList()
                    let lst2 = EmptyList()
                    OOPList_MyList(сoncatenation lst1 lst2)

                Expect.equal actualResult MyListHW.Empty "The result should be MyListHW.Empty"

            testCase "2 test for concatenation OL"
            <| fun _ ->
                let actualResult =
                    let lst1 = List(5, EmptyList())
                    let lst2 = List(5, EmptyList())
                    OOPList_MyList(сoncatenation lst1 lst2)

                Expect.equal
                    actualResult
                    (MyListHW.Cons(5, MyListHW.Cons(5, MyListHW.Empty)))
                    "The result should be MyListHW.Cons(5, MyListHW.Cons(5, MyListHW.Empty)))"

            testCase "1 test for BubbleSort OL"
            <| fun _ ->
                let actualResult =
                    let lst = List(1, List(2, EmptyList()))
                    OOPList_MyList(bubbleSort lst)

                Expect.equal
                    actualResult
                    (MyListHW.Cons(1, MyListHW.Cons(2, MyListHW.Empty)))
                    "The result should be MyListHW.Cons(1, MyListHW.Cons(2, MyListHW.Empty))"

            testCase "2 test for BubbleSort OL"
            <| fun _ ->
                let actualResult =
                    let lst = List("a", List("b", EmptyList()))
                    OOPList_MyList(bubbleSort lst)

                Expect.equal
                    actualResult
                    (MyListHW.Cons("a", MyListHW.Cons("b", MyListHW.Empty)))
                    "The result should be MyListHW.Cons(1, MyListHW.Cons(2, MyListHW.Empty))"

            testCase "1 test for QuickSort OL"
            <| fun _ ->
                let actualResult =
                    let lst = List(1, List(2, EmptyList()))
                    OOPList_MyList(quickSort lst)

                Expect.equal
                    actualResult
                    (MyListHW.Cons(1, MyListHW.Cons(2, MyListHW.Empty)))
                    "The result should be MyListHW.Cons(1, MyListHW.Cons(3, MyListHW.Empty))"

            testCase "2 test for QuickSort OL"
            <| fun _ ->
                let actualResult =
                    let lst = List("a", List("b", EmptyList()))
                    OOPList_MyList(quickSort lst)

                Expect.equal
                    actualResult
                    (MyListHW.Cons("a", MyListHW.Cons("b", MyListHW.Empty)))
                    "The result should be MyListHW.Cons('a', MyListHW.Cons('b', MyListHW.Empty))"
        ]
