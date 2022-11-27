namespace TreeHomeworks.Tests

open FsCheck
open Expecto
open MyListHW
open TreesHW

module MyList =
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "UniqueElements test. The tree has 1 node"
            <| fun _ ->
                let actualResult = uniqueElements (Leaf(5))
                Expect.equal actualResult 1 "The tree with 1 node has 1 unique element"

            testCase "UniqueElements test. The tree has random int values"
            <| fun _ ->
                let actualResult =
                    uniqueElements (
                        Node(
                            10,
                            [|
                                Node(7, [| Leaf(10) |])
                                Node(3, [| Leaf(9) |])
                                Node(5, [| Node(5, [| Leaf(11) |]) |])
                            |]
                        )
                    )

                Expect.equal actualResult 6 "The result should be 6"

            testCase "UniqueElements test. The tree has random string values"
            <| fun _ ->
                let actualResult =
                    uniqueElements (
                        Node(
                            "aa",
                            [|
                                Node("ab", [| Leaf("ba") |])
                                Node("bb", [| Leaf("aa") |])
                            |]
                        )
                    )

                Expect.equal actualResult 4 "The result should be 4"

            testCase "UniqueElements test. The tree has the same values"
            <| fun _ ->
                let actualResult = uniqueElements (Node(5, [| Leaf(5) |]))
                Expect.equal actualResult 1 "The result should be 1"

            testCase "MyList of tree with 1 node"
            <| fun _ ->
                let actualResult = myList_tree (Leaf(5))
                Expect.equal actualResult (Cons(5, Empty)) "The result should be Cons(5, Empty)"

            testProperty "Length of MyList"
            <| fun (tree: SomeTree<int>) ->
                Expect.isGreaterThan
                <| lenght (myList_tree tree)
                <| 0
                <| "The length of MyList cannot be 0"

            testProperty "the length of MyList"
            <| fun (tree: SomeTree<int>) ->
                Expect.isGreaterThanOrEqual
                <| lenght (myList_tree tree)
                <| uniqueElements tree
                <| "the length of MyList cannot be less than the number of different elements"
        ]
