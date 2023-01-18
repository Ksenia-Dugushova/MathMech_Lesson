module SparseVector

open System

type Vector<'Value> =
    struct
        val Memory: array<'Value option>
        val Head: uint
        val Lenght: uint


        new(memory, head, length) =
            { Memory = memory
              Head = head
              Lenght = length }
    end

type BinaryTree<'Value> =
    | None
    | Leaf of 'Value
    | Node of BinaryTree<'Value> * BinaryTree<'Value>

let square (arr: option<'Value>[]) =
    let length = arr.Length
    let logarithm = Math.Log(length, 2)

    if (ceil logarithm) = logarithm then
        uint length
    else
        uint (2.0 ** ceil logarithm)

let squareValue (length: uint) =
    let logarithm = Math.Log(float length, 2)

    if (ceil logarithm) = logarithm then
        length
    else
        uint (2.0 ** ceil logarithm)

let toBinaryTree arr =
    let optionToBinaryTree optionValue =
        match optionValue with
        | Option.None -> BinaryTree.None
        | Some value -> BinaryTree.Leaf(value)

    let rec binaryTreeMaking (vec: Vector<'Value>) =
        let head = vec.Head
        let length = vec.Lenght
        let memory = vec.Memory
        let realLength = memory.Length

        if head >= uint realLength then
            BinaryTree.None
        elif length = 1u then
            optionToBinaryTree memory[Convert.ToInt32(head)]
        else
            let left = binaryTreeMaking (Vector(memory, head, length / 2u))
            let right = binaryTreeMaking (Vector(memory, head + length / 2u, length / 2u))

            if left = BinaryTree.None && right = BinaryTree.None then
                BinaryTree.None
            else
                Node(left, right)

    binaryTreeMaking (Vector(arr, 0u, square arr))

let noneDestroyer (tree: BinaryTree<'Value>) =
    match tree with
    | Leaf value -> Leaf value
    | Node (None, None) -> None
    | _ -> tree

let partition (list: List<uint * 'A>) length =
    let rec f list left right =
        match list with
        | [] -> left, right
        | (x, y) :: tail ->
            if x < length / 2u then
                f tail ((x, y) :: left) right
            else
                f tail left ((x - length / 2u, y) :: right)

    f list [] []

let closestDegreeOf2 (columns: uint) (lines: uint) =
    uint (2.0 ** Math.Ceiling(Math.Log(float (max columns lines), 2.0)))

let toBinTreeFromCOO list size =
    let virtualLength = closestDegreeOf2 size 0u

    let rec helper list virtualLength =
        if virtualLength = 0u then
            BinaryTree.None
        elif virtualLength = 1u && not (List.isEmpty list) && (fst list.Head < size) then
            BinaryTree.Leaf(snd list.Head)
        elif virtualLength = 1u && ((List.isEmpty list) || (fst list.Head >= size)) then
            BinaryTree.None
        else
            let lists = partition list virtualLength

            BinaryTree.Node(helper (fst lists) (virtualLength / 2u), helper (snd lists) (virtualLength / 2u))
            |> noneDestroyer

    helper list virtualLength

let ceilPowTwo a =
    let rec looper a acc =
        if acc >= a then acc else looper a (acc * 2u)

    if a = 0u then 1u
    elif a = 1u then 2u
    else looper a 1u


type SparseVector<'Value when 'Value: equality> =
    val Storage: BinaryTree<'Value>
    val Length: uint

    new(arr) =
        { Storage = toBinaryTree arr
          Length = uint arr.Length }

    new(storage, length) = { Storage = storage; Length = length }

    new(list, realLength) =
        { Storage = toBinTreeFromCOO list realLength
          Length = realLength }

    member this.Item
        with get i =
            let vectorElement i (vector: SparseVector<'Value>) =
                let rec element i size tree =
                    match tree with
                    | BinaryTree.Leaf value -> Some(value)
                    | BinaryTree.None -> Option.None
                    | BinaryTree.Node (left, right) ->
                        let middle = size / 2u

                        if i < middle then
                            element i middle left
                        else
                            element (i - middle) middle right

                if i < vector.Length then
                    let powerSize = ceilPowTwo vector.Length
                    element i powerSize vector.Storage
                else
                    failwith $"Index out of the range"

            vectorElement i this

    member this.IsEmpty =
        match this.Storage with
        | BinaryTree.None -> true
        | _ -> false

type COOVector<'A>(list: List<uint * 'A>, length: uint) =
    member this.Data = list
    member this.Length = length

let valueOrNone z =
    match z with
    | Option.None -> BinaryTree.None
    | Some value -> BinaryTree.Leaf value

let noneBreak (tree: BinaryTree<'Value>) =
    match tree with
    | Leaf value -> Leaf value
    | Node (None, None) -> None
    | _ -> tree

let addVector (fPlus: option<'Value1> -> option<'Value2> -> option<'Value3>) (vector1: SparseVector<'Value1>) (vector2: SparseVector<'Value2>) : SparseVector<'Value3> =
    let rec addTrees (tree1: BinaryTree<'Value1>) (tree2: BinaryTree<'Value2>) : BinaryTree<'Value3> =
        match tree1, tree2 with
        | None, None -> BinaryTree.None
        | Leaf value1, Leaf value2 -> fPlus (Some value1) (Some value2) |> valueOrNone
        | None, Leaf value -> fPlus Option.None (Some value) |> valueOrNone
        | Leaf value, None -> fPlus (Some value) Option.None |> valueOrNone
        | None, Node (left, right) -> Node(addTrees None left, addTrees None right) |> noneBreak
        | Node (left, right), None -> Node(addTrees left None, addTrees right None) |> noneBreak
        | Node (left, right), Node (left2, right2) -> Node(addTrees left left2, addTrees right right2) |> noneBreak
        | _, _ -> failwith $"Something going wrong"

    if vector1.Length = vector2.Length then
        SparseVector(addTrees vector1.Storage vector2.Storage, vector1.Length)
    else
        failwith $"The lengths of the vectors are not equal"
