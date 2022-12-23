module SparseVector

open System

type Vector<'value> =
    struct
        val Memory: array<Option<'value>>
        val Head: int
        val Lenght: int

        new(memory, head, length) =
            { Memory = memory
              Head = head
              Lenght = length }
    end

type BinaryTree<'value> =
    | None
    | Leaf of 'value
    | Node of BinaryTree<'value> * BinaryTree<'value>

let square (arr: 'value option[]) =
    let length = arr.Length
    let logarithm = Math.Log(length, 2)

    if (ceil logarithm) = logarithm then
        length
    else
        int (2.0 ** ceil logarithm)

let toBinaryTree arr =
    let optionToBinaryTree optionValue =
        match optionValue with
        | Option.None -> BinaryTree.None
        | Some value -> BinaryTree.Leaf(value)

    let rec binaryTreeMaking (vec: Vector<'value>) =
        let head = vec.Head
        let length = vec.Lenght
        let memory = vec.Memory
        let realLength = memory.Length

        if head >= realLength then
            BinaryTree.None
        elif length = 1 then
            optionToBinaryTree memory[head]
        else
            let left = binaryTreeMaking (Vector(memory, head, length / 2))
            let right = binaryTreeMaking (Vector(memory, head + length / 2, length / 2))

            if left = BinaryTree.None && right = BinaryTree.None then
                BinaryTree.None
            else
                Node(left, right)

    binaryTreeMaking (Vector(arr, 0, square arr))

type SparseVector<'value when 'value: equality> =
    struct
        val Storage: BinaryTree<'value>
        val Length: int
        val LengthSquare: int

        new(storage, length, lengthSquare) =
            { Storage = storage
              Length = length
              LengthSquare = lengthSquare }

        new(arr: array<Option<'value>>) =
            { Storage = toBinaryTree arr
              Length = arr.Length
              LengthSquare = square arr }

        member this.Item
            with get i =
                let vectorElement i (vector: SparseVector<'value>) =
                    let rec element i size tree =
                        match tree with
                        | BinaryTree.Leaf value -> Some(value)
                        | BinaryTree.None -> Option.None
                        | BinaryTree.Node (left, right) ->
                            let middle = size / 2

                            if i < middle then
                                element i middle left
                            else
                                element (i - middle) middle right

                    if i < vector.Length then
                        element i vector.LengthSquare vector.Storage
                    else
                        failwith "Index out of the range"

                vectorElement i this
    end


let addVector (fPlus: 'value1 option -> 'value2 option -> 'value3 option) (vector1: SparseVector<'value1>) (vector2: SparseVector<'value2>) : SparseVector<'value3> =
    let f x y =
        let z = fPlus x y

        match z with
        | Option.None -> BinaryTree.None
        | Some z -> BinaryTree.Leaf z

    let rec operationTree tree1 tree2 =
        match tree1, tree2 with
        | BinaryTree.Leaf x, BinaryTree.Leaf y -> f (Some x) (Some y)
        | BinaryTree.None, x ->
            match x with
            | Leaf a -> f Option.None (Some a)
            | BinaryTree.None -> BinaryTree.None
            | BinaryTree.Node (a, b) -> operationTree (BinaryTree.Node(BinaryTree.None, BinaryTree.None)) (BinaryTree.Node(a, b))

        | x, BinaryTree.None ->
            match x with
            | Leaf a -> f (Some a) Option.None
            | BinaryTree.None -> BinaryTree.None
            | BinaryTree.Node (a, b) -> operationTree (BinaryTree.Node(a, b)) (BinaryTree.Node(BinaryTree.None, BinaryTree.None))

        | BinaryTree.Node (x, y), BinaryTree.Node (z, w) ->
            let left = operationTree x z
            let right = operationTree y w

            if left = BinaryTree.None && right = BinaryTree.None then
                BinaryTree.None
            else
                BinaryTree.Node(left, right)
        | BinaryTree.Node (x, y), BinaryTree.Leaf z -> operationTree <| BinaryTree.Node(x, y) <| BinaryTree.Node(BinaryTree.Leaf z, BinaryTree.Leaf z)
        | BinaryTree.Leaf z, BinaryTree.Node (x, y) -> operationTree <| BinaryTree.Node(BinaryTree.Leaf z, BinaryTree.Leaf z) <| BinaryTree.Node(x, y)

    if vector1.Length = vector2.Length then
        SparseVector(operationTree vector1.Storage vector2.Storage, vector1.Length, vector1.LengthSquare)
    else
        failwith "The lengths of the vectors are not equal"
