module Test4

open FsCheck
open Expecto
open System
open Microsoft.FSharp.Core

module SparseVectorTests =
    open SparseVector

    [<Tests>]
    let tests =
        testList "samples" [
            testCase "function square for one element 1"
            <| fun _ ->
                let actualResult = square [| Some(1) |]
                Expect.equal actualResult 1 "The result should be 1"

            testCase "function square for one element 2"
            <| fun _ ->
                let actualResult =
                    square [|
                        Some(11)
                        Some(5)
                        Some(26)
                        Some(2)
                        Some(9)
                        Some(33)
                        Some(4)
                        Some(17)
                        Some(8)
                    |]

                Expect.equal actualResult 16 "The result should be 16"

            testCase "function square for empty array"
            <| fun _ ->
                let actualResult = square [||]
                Expect.equal actualResult 0 "The result should be 0"

            testProperty "function square property test"
            <| fun (arr: array<Option<int>>) ->
                Expect.isLessThanOrEqual
                <| arr.Length
                <| square arr
                <| "Square expected less than or equal array length result"

            testCase "toBinaryTree for None array"
            <| fun _ ->
                let actualResult = toBinaryTree [| Option.None |]
                Expect.equal actualResult BinaryTree.None "The result should be BinaryTree.None"

            testCase "toBinaryTree for random array"
            <| fun _ ->
                let actualResult =
                    toBinaryTree [|
                        Some(1)
                        Some(1)
                        Option.None
                        Option.None
                    |]

                Expect.equal
                    actualResult
                    (Node(Node(Leaf(1), Leaf(1)), BinaryTree.None))
                    "The result should be (Node(Node(Leaf(1), Leaf(1))"

            testCase "toBinaryTree for string array"
            <| fun _ ->
                let actualResult =
                    toBinaryTree [|
                        Some("a")
                        Some("bb")
                    |]

                Expect.equal
                    actualResult
                    (Node(Leaf("a"), Leaf("bb")))
                    "The result should be (Node(Leaf('a'), Leaf('bb')))"

            testCase "vectorElement for empty array"
            <| fun _ ->
                let actualResult =
                    Expect.throws
                        (fun _ ->
                            SparseVector([||])[152]
                            |> ignore
                        )
                        "Index out of the range"

                actualResult

            testProperty "vectorElement property test"
            <| fun (arr: array<Option<int>>) (i: uint) ->
                let arr' = Array.append arr [| Some(1) |]
                let i' = int i % arr'.Length

                Expect.equal
                <| arr'[int i']
                <| SparseVector(arr')[i']
                <| "vectorElement expected same result as Array.get"

            testProperty "Operation property test"
            <| fun (x: int) ->
                let length1 =
                    (abs x)
                    + 1

                let rnd = System.Random()
                let arr1 = Array.init length1 (fun _ -> rnd.Next(100))

                let arr1Some =
                    arr1
                    |> Array.map (fun middle -> if middle % 2 = 0 then Some middle else Option.None)

                let arr2 = Array.init length1 (fun _ -> rnd.Next(100))

                let arr2Some =
                    arr2
                    |> Array.map (fun n -> if n % 2 = 0 then Some n else Option.None)

                let vector1 = SparseVector(arr1Some)
                let vector2 = SparseVector(arr2Some)

                let funPlus opt1 opt2 =
                    match opt1, opt2 with
                    | Option.Some a, Option.Some b -> Option.Some(a + b)
                    | Option.Some a, Option.None
                    | Option.None, Option.Some a -> Option.Some(a)
                    | Option.None, Option.None -> Option.None

                let naiveAddition (arr1: array<Option<int>>) (arr2: array<Option<int>>) =

                    let length = arr1.Length
                    let mutable res = Array.zeroCreate length

                    for i in 0 .. length - 1 do
                        res[i] <- funPlus arr1[i] arr2[i]

                    res

                let expectedResult = SparseVector(naiveAddition arr1Some arr2Some)
                let actualResult = operation funPlus vector1 vector2
                Expect.equal actualResult.Keeping expectedResult.Keeping "Undefined result. "
        ]

module SparseMatrixTests =
    open SparseMatrix

    [<Tests>]
    let tests =
        testList "samples" [
            testCase "Square for array 2D 1"
            <| fun _ ->
                let actualResult =
                    square (
                        array2D [
                            [
                                Some(1)
                                Some(2)
                            ]
                            [
                                Some(3)
                                Some(4)
                            ]
                        ]
                    )

                Expect.equal actualResult 2 "The result should be 2"

            testCase "Square for array 2D 2"
            <| fun _ ->
                let actualResult =
                    square (
                        array2D [
                            [
                                Some(7)
                                Some(22)
                                Some(3)
                            ]
                            [
                                Some(4)
                                Some(51)
                                Some(21)
                            ]
                            [
                                Some(3)
                                Some(34)
                                Some(35)
                            ]
                        ]
                    )

                Expect.equal actualResult 4 "The result should be 4"

            testCase "Square for array 2D with one element"
            <| fun _ ->
                let actualResult = square (array2D [ [ Some(10) ] ])
                Expect.equal actualResult 1 "The result should be 1"

            testCase "toQuadTree for empty array 2D"
            <| fun _ ->
                let actualResult = toQuadTree (array2D [ [] ])
                Expect.equal actualResult QuadTree.None "The result should be QuadTree.None"

            testCase "toQuadTree for array2D "
            <| fun _ ->
                let tree =
                    toQuadTree (
                        array2D [
                            [
                                Some(1)
                                Some(1)
                            ]
                            [
                                Option.None
                                Option.None
                            ]
                            [
                                Some(1)
                                Some(1)
                            ]
                        ]
                    )

                Expect.equal
                    tree
                    (Node(
                        Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None),
                        QuadTree.None,
                        Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None),
                        QuadTree.None
                    ))
                    "toQuadTree expected : (Node(Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None, Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None))"

            testProperty "toSquare property test array2D"
            <| fun (arr: int option[,]) ->
                Expect.isLessThanOrEqual
                <| max (Array2D.length1 arr) (Array2D.length2 arr)
                <| square arr
                <| "square expected less than or equal array length result"

            testProperty "elementOfMatrix property test"
            <| fun (arr: int option[,]) (i: uint) (j: uint) ->
                let arr' =
                    if
                        Array2D.length1 arr = 0
                        || Array2D.length2 arr = 0
                    then
                        (array2D [
                            [
                                Some(1)
                                Some(2)
                            ]
                            [
                                Some(2)
                                Some(2)
                            ]
                        ])
                    else
                        arr

                let i' = int i % (Array2D.length1 arr')
                let j' = int j % (Array2D.length2 arr')

                Expect.equal
                <| arr'[int i', int j']
                <| SparseMatrix(arr')[i', j']
                <| "Something went wrong"
        ]

module MultiMatrixTests =
    open MultiplicationMatrix
    open SparseMatrix
    open SparseVector

    let funPlusInt a b =
        match a, b with
        | Some x, Some y -> Some(x + y)
        | Option.None, Some x
        | Some x, Option.None -> Some x
        | Option.None, Option.None -> Option.None

    let funMultiInt a b =
        match a, b with
        | Some x, Some y -> Some(x * y)
        | Option.None, _
        | _, Option.None -> Option.None

    [<Tests>]
    let tests =
        testList "samples" [
            testCase "Multi vector and matrix"
            <| fun _ ->
                let vec =
                    SparseVector(
                        [|
                            Some(0)
                            Some(1)
                        |]
                    )

                let mat =
                    SparseMatrix(
                        array2D [
                            [
                                Some(1)
                                Some(1)
                            ]
                            [
                                Some(1)
                                Some(2)
                            ]
                        ]
                    )

                let res = multiplication funPlusInt funMultiInt vec mat

                Expect.equal
                    res.Keeping
                    (BinaryTree.Node(BinaryTree.Leaf(1), BinaryTree.Leaf(2)))
                    "The result should be (BinaryTree.Node(BinaryTree.Leaf(1), BinaryTree.Leaf(2)))"

            testCase "Multi None vector and matrix"
            <| fun _ ->
                let vec =
                    SparseVector(
                        [|
                            Some(1)
                            Some(1)
                            Some(1)
                        |]
                    )

                let mat =
                    SparseMatrix(
                        array2D [
                            [
                                Option.None
                                Option.None
                                Option.None
                            ]
                            [
                                Option.None
                                Option.None
                                Option.None
                            ]
                            [
                                Option.None
                                Option.None
                                Option.None
                            ]
                        ]
                    )

                let res = multiplication funPlusInt funMultiInt vec mat
                Expect.equal res.Keeping BinaryTree.None "The result should be BinaryTree.None"

            testProperty "Property test for multiplication"
            <| fun (x: int) (y: int) ->
                let length1 =
                    (abs x)
                    + 1

                let length2 =
                    (abs y)
                    + 1

                let rnd = System.Random()
                let arr = Array.init length1 (fun _ -> rnd.Next(100))

                let arrSome =
                    arr
                    |> Array.map (fun n -> if n % 2 = 0 then Some n else Option.None)

                let arr2d = Array2D.init length1 length2 (fun _ _ -> rnd.Next(100))

                let arr2dSome =
                    arr2d
                    |> Array2D.map (fun n -> if n % 2 = 0 then Some n else Option.None)

                let vector = SparseVector(arrSome)
                let matrix = SparseMatrix(arr2dSome)

                let naiveMulti (arr: int option[]) (arr2d: int option[,]) =
                    let funPlus opt1 opt2 =
                        match opt1, opt2 with
                        | Option.Some a, Option.Some b -> Option.Some(a + b)
                        | Option.Some a, Option.None
                        | Option.None, Option.Some a -> Option.Some(a)
                        | Option.None, Option.None -> Option.None

                    let funMulti opt1 opt2 =
                        match opt1, opt2 with
                        | Option.Some a, Option.Some b -> Option.Some(a * b)
                        | _, Option.None
                        | Option.None, _ -> Option.None

                    let rows = arr.Length
                    let columns = Array2D.length2 arr2d
                    let mutable res = Array.zeroCreate columns

                    for j in
                        0 .. columns
                             - 1 do
                        for i in 0 .. rows - 1 do
                            res[j] <- funPlus res[j] (funMulti arr[i] arr2d[i, j])

                    res

                let rec isNoneReduce tree =
                    match tree with
                    | BinaryTree.Node (BinaryTree.None, BinaryTree.None) -> false
                    | BinaryTree.Node (x, y) ->
                        isNoneReduce x
                        && isNoneReduce y
                    | BinaryTree.Leaf _ -> true
                    | BinaryTree.None -> true

                let expectedResult = SparseVector(naiveMulti arrSome arr2dSome)
                let actualResult = multiplication funPlusInt funMultiInt vector matrix
                let actualResult' = isNoneReduce actualResult.Keeping

                Expect.equal
                    actualResult.Keeping
                    expectedResult.Keeping
                    $"\n Array : %A{arrSome}\n
                       \n Array2d : %A{arr2dSome}\n
                       \n Array tree : %A{vector.Keeping}\n
                       \n Matrix tree : %A{matrix.Keeping}\n. "

                Expect.equal
                    actualResult.Length
                    matrix.LengthR
                    "The actualResult.Length should be the same matrix.Length1. "

                Expect.equal actualResult' true "Something went wrong"
        ]
