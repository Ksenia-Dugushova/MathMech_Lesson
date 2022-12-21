module MultiplicationMatrix

open SparseMatrix
open SparseVector
open System

let multiplication
    plus
    (multiOperation: 'value1 option -> 'value2 option -> 'value3 option)
    (vector: SparseVector<'value1>)
    (matrix: SparseMatrix<'value2>)
    : SparseVector<'value3> =
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
                (operation
                 <| plus
                 <| SparseVector(multiTree left fst, vector.Length, vector.LengthSquare)
                 <| SparseVector(multiTree right thd, vector.Length, vector.LengthSquare))
                    .Keeping

            let snd =
                (operation
                 <| plus
                 <| SparseVector(multiTree left snd, vector.Length, vector.LengthSquare)
                 <| SparseVector(multiTree right fth, vector.Length, vector.LengthSquare))
                    .Keeping

            if
                fst = BinaryTree.None
                && snd = BinaryTree.None
            then
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
            <| QuadTree.Node(
                QuadTree.Leaf value,
                QuadTree.Leaf value,
                QuadTree.Leaf value,
                QuadTree.Leaf value
            )

    let rec splitBinaryTree (tree: BinaryTree<'value>) desiredSize splitSize =
        match tree with
        | BinaryTree.Node (fst, _) when
            desiredSize
            <> splitSize
            ->
            splitBinaryTree
                fst
                desiredSize
                (splitSize
                 / 2)
        | _ -> tree

    let rec makingBinareTree (tree: BinaryTree<'vslue>) desiredSize splitSize =
        if
            desiredSize
            <> splitSize
        then
            makingBinareTree
                (BinaryTree.Node(tree, BinaryTree.None))
                desiredSize
                (splitSize
                 * 2)
        else
            tree

    if vector.Length = matrix.LengthR then
        let makeTree =
            if
                vector.LengthSquare
                <> matrix.LengthSquare
            then
                multiTree
                    (makingBinareTree vector.Keeping matrix.LengthSquare vector.LengthSquare)
                    matrix.Keeping
            else
                multiTree vector.Keeping matrix.Keeping

        let splitTree =
            if matrix.LengthR > matrix.LengthC then
                let desiredSize =
                    int (
                        2.0
                        ** ceil (Math.Log(matrix.LengthC, 2))
                    )

                splitBinaryTree makeTree desiredSize matrix.LengthSquare
            else
                makeTree

        SparseVector(splitTree, vector.Length, vector.Length)
    else
        failwith "Something went wrong"
