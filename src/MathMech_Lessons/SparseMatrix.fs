module SparseMatrix

open System
open SparseVector

type QuadTree<'value> =
    | None
    | Leaf of 'value
    | Node of QuadTree<'value> * QuadTree<'value> * QuadTree<'value> * QuadTree<'value>

let square arr =
    let lengthR = Array2D.length1 arr
    let lengthC = Array2D.length2 arr
    let logarithm1 = Math.Log(lengthR, 2)
    let logarithm2 = Math.Log(lengthC, 2)

    if ceil logarithm1 = logarithm1 && ceil logarithm2 = logarithm2 && logarithm1 = logarithm2 then
        lengthR
    else
        int (2.0 ** ceil (max logarithm1 logarithm2))

type SquareArray<'value> =
    struct
        val Memory: 'value option[,]
        val HeadOfRow: int
        val HeadOfColumn: int
        val Length: int

        new(memory, headOfRow, headOfColumn, length) =
            { Memory = memory
              HeadOfRow = headOfRow
              HeadOfColumn = headOfColumn
              Length = length }
    end

let toQuadTree arr =
    let optionToQuadTree optionValue =
        match optionValue with
        | Option.None -> QuadTree.None
        | Some value -> QuadTree.Leaf(value)


    let rec quadTreeMaking (arr: SquareArray<'value>) =
        let memory = arr.Memory
        let head1 = arr.HeadOfRow
        let head2 = arr.HeadOfColumn
        let length = arr.Length
        let realLengthR = Array2D.length1 memory
        let realLengthC = Array2D.length2 memory

        if head1 >= realLengthR || head2 >= realLengthC then
            QuadTree.None
        elif length = 1 then
            optionToQuadTree memory[head1, head2]
        else
            let fst = quadTreeMaking (SquareArray(memory, head1, head2, length / 2))

            let snd =
                quadTreeMaking (SquareArray(memory, head1, head2 + length / 2, length / 2))

            let thd =
                quadTreeMaking (SquareArray(memory, head1 + length / 2, head2, length / 2))

            let fth =
                quadTreeMaking (SquareArray(memory, head1 + length / 2, head2 + length / 2, length / 2))

            if fst = QuadTree.None && snd = QuadTree.None && thd = QuadTree.None && fth = QuadTree.None then
                QuadTree.None
            else
                Node(fst, snd, thd, fth)

    quadTreeMaking (SquareArray(arr, 0, 0, square arr))

type SparseMatrix<'value when 'value: equality> =
    struct
        val Storage: QuadTree<'value>
        val RowCount: int
        val ColumnCount: int
        val LengthSquare: int

        new(storage, rowCount, columnCount, lengthSquare) =
            { Storage = storage
              RowCount = rowCount
              ColumnCount = columnCount
              LengthSquare = lengthSquare }

        new(arr: 'value option[,]) =
            { Storage = toQuadTree arr
              RowCount = Array2D.length1 arr
              ColumnCount = Array2D.length2 arr
              LengthSquare = square arr }

        member this.Item
            with get (a, b) =
                let matrixElement a b (matrix: SparseMatrix<'value>) =
                    let rec element a b size tree =
                        match tree with
                        | QuadTree.Leaf value -> Some(value)
                        | QuadTree.None -> Option.None
                        | QuadTree.Node (x, y, z, w) ->
                            let middle = size / 2

                            if a < middle && b < middle then
                                element a b middle x
                            elif a < middle && b >= middle then
                                element a (b - middle) middle y
                            elif a >= middle && b < middle then
                                element (a - middle) b middle z
                            else
                                element (a - middle) (b - middle) middle w

                    if a < matrix.RowCount && b < matrix.ColumnCount then
                        element a b matrix.LengthSquare matrix.Storage
                    else
                        failwith "Index out of the range"

                matrixElement a b this
    end
