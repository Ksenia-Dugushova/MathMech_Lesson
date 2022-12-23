module MultiplicationMatrix

open SparseMatrix
open SparseVector
open System

let multiplication plus (multiOperation: option<'value1> -> option<'value2> -> option<'value3>) (vector: SparseVector<'value1>) (matrix: SparseMatrix<'value2>) : SparseVector<'value3> =
    let rec multiTree binaryTree quadTree =
        match binaryTree, quadTree with
        | BinaryTree.None, _
        | _, QuadTree.None -> BinaryTree.None
        | BinaryTree.Leaf a, QuadTree.Leaf b ->
            let c = multiOperation (Some a) (Some b)

            match c with
            | Some c -> BinaryTree.Leaf c
            | Option.None -> BinaryTree.None
        | BinaryTree.Node (left, right), QuadTree.Node (fst, snd, thd, fth) ->
            let fst =
                (addVector
                 <| plus
                 <| SparseVector(multiTree left fst, vector.Length, vector.LengthSquare)
                 <| SparseVector(multiTree right thd, vector.Length, vector.LengthSquare))
                    .Storage

            let snd =
                (addVector
                 <| plus
                 <| SparseVector(multiTree left snd, vector.Length, vector.LengthSquare)
                 <| SparseVector(multiTree right fth, vector.Length, vector.LengthSquare))
                    .Storage

            if fst = BinaryTree.None && snd = BinaryTree.None then
                BinaryTree.None
            else
                BinaryTree.Node(fst, snd)
        | BinaryTree.Leaf value, QuadTree.Node (fst, snd, thd, fth) ->
            multiTree
            <| BinaryTree.Node(BinaryTree.Leaf value, BinaryTree.Leaf value)
            <| QuadTree.Node(fst, snd, thd, fth)
        | BinaryTree.Node (left, right), QuadTree.Leaf value ->
            multiTree
            <| BinaryTree.Node(left, right)
            <| QuadTree.Node(QuadTree.Leaf value, QuadTree.Leaf value, QuadTree.Leaf value, QuadTree.Leaf value)

    let rec CutBinaryTree (tree: BinaryTree<'value>) desiredSize currentSize =
        match tree with
        | BinaryTree.Node (fst, _) when desiredSize <> currentSize -> CutBinaryTree fst desiredSize (currentSize / 2)
        | _ -> tree

    let rec binareTreeMaker (tree: BinaryTree<'value>) desiredSize currentSize =
        if desiredSize <> currentSize then
            binareTreeMaker (BinaryTree.Node(tree, BinaryTree.None)) desiredSize (currentSize * 2)
        else
            tree

    if vector.Length = matrix.RowCount then
        let makeTree =
            if vector.LengthSquare <> matrix.LengthSquare then
                multiTree (binareTreeMaker vector.Storage matrix.LengthSquare vector.LengthSquare) matrix.Storage
            else
                multiTree vector.Storage matrix.Storage

        let splitTree =
            if matrix.RowCount > matrix.ColumnCount then
                let desiredSize = int (2.0 ** ceil (Math.Log(matrix.ColumnCount, 2)))

                CutBinaryTree makeTree desiredSize matrix.LengthSquare
            else
                makeTree

        SparseVector(splitTree, vector.Length, vector.Length)
    else
        failwith "Something went wrong"
