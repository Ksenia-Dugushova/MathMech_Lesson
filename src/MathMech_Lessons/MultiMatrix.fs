module MultiMatrix

open SparseMatrix
open SparseVector
open System

let multiplication plus (multiOperation: option<'Value1> -> option<'Value2> -> option<'Value3>) (vector: SparseVector<'Value1>) (matrix: SparseMatrix<'Value2>) : SparseVector<'Value3> =
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
                 <| SparseVector(multiTree left fst, vector.Length)
                 <| SparseVector(multiTree right thd, vector.Length))
                    .Storage

            let snd =
                (addVector
                 <| plus
                 <| SparseVector(multiTree left snd, vector.Length)
                 <| SparseVector(multiTree right fth, vector.Length))
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

    let rec cutBinaryTree (tree: BinaryTree<'Value>) desiredSize currentSize =
        match tree with
        | BinaryTree.Node (fst, _) when desiredSize <> currentSize -> cutBinaryTree fst desiredSize (currentSize / 2u)
        | _ -> tree

    let rec binareTreeMaker (tree: BinaryTree<'Value>) desiredSize splitSize =
        if desiredSize <> splitSize then
            binareTreeMaker (BinaryTree.Node(tree, BinaryTree.None)) desiredSize (splitSize * 2u)
        else
            tree

    let powerSizeV = ceilPowTwo vector.Length
    let powerSizeM = ceilPowTwo (max matrix.RowCount matrix.ColumnCount)

    if vector.Length = matrix.RowCount then
        let makeTree =
            if powerSizeV <> powerSizeM then
                multiTree (binareTreeMaker vector.Storage powerSizeM powerSizeV) matrix.Storage
            else
                multiTree vector.Storage matrix.Storage

        let cutTree =
            if matrix.RowCount > matrix.ColumnCount then
                let desiredSize = uint (2.0 ** ceil (Math.Log(float (matrix.ColumnCount), 2)))
                cutBinaryTree makeTree desiredSize powerSizeM
            else
                makeTree

        SparseVector(cutTree, vector.Length)
    else
        failwith $"Something went wrong"
