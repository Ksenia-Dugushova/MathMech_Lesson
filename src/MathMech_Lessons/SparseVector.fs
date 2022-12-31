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

let square (arr: option<'value>[]) =
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

let ceilPowTwo a =
    let rec looper a acc =
        if acc >= a then acc else looper a (acc * 2)

    if a <= 0 then 1
    elif a = 1 then 2
    else looper a 1

type SparseVector<'value when 'value: equality> =
    struct
        val Storage: BinaryTree<'value>
        val Length: int
        new(storage, length) = { Storage = storage; Length = length }

        new(arr: array<Option<'value>>) =
            { Storage = toBinaryTree arr
              Length = arr.Length }

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
                        let powerSize = ceilPowTwo vector.Length
                        element i powerSize vector.Storage
                    else
                        failwith "Index out of the range"

                vectorElement i this
    end

let valueOrNone z =
    match z with
    | Option.None -> BinaryTree.None
    | Some value -> BinaryTree.Leaf value

let NoneBreak (tree: BinaryTree<'value>) =
    match tree with
    | Leaf value -> Leaf value
    | Node (None, None) -> None
    | _ -> tree

let addVector (fPlus: option<'value1> -> option<'value2> -> option<'value3>) (vector1: SparseVector<'value1>) (vector2: SparseVector<'value2>) : SparseVector<'value3> =
    let rec addTrees (tree1: BinaryTree<'value1>) (tree2: BinaryTree<'value2>) : BinaryTree<'value3> =
        match tree1, tree2 with
        | None, None -> BinaryTree.None
        | Leaf value1, Leaf value2 -> fPlus (Some value1) (Some value2) |> valueOrNone
        | None, Leaf value -> fPlus Option.None (Some value) |> valueOrNone
        | Leaf value, None -> fPlus (Some value) Option.None |> valueOrNone
        | None, Node (left, right) -> Node(addTrees None left, addTrees None right) |> NoneBreak
        | Node (left, right), None -> Node(addTrees left None, addTrees right None) |> NoneBreak
        | Node (left, right), Node (left2, right2) -> Node(addTrees left left2, addTrees right right2) |> NoneBreak
        | _, _ -> failwith $"Something going wrong"

    if vector1.Length = vector2.Length then
        SparseVector(addTrees vector1.Storage vector2.Storage, vector1.Length)
    else
        failwith "The lengths of the vectors are not equal"
