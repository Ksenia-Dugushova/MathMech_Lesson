module BFSparent

open SparseVector
open SparseMatrix
open MultiMatrix
open FSharp.Collections
open System


let mult x y =
    match x, y with
    | Some value, Some _ -> Some value
    | _ -> Option.None

let add x y =
    match x, y with
    | Option.None, Option.None -> Option.None
    | Option.None, Some value
    | Some value, Option.None -> Some value
    | Some value1, Some value2 -> Some(min value1 value2)

let mask x y =
    match x, y with
    | Some value, Option.None -> Some value
    | _ -> Option.None

let plusVisited x y =
    match x, y with
    | Option.None, Option.None -> Option.None
    | Option.None, Some value
    | Some value, Option.None -> Some value
    | Some _, Some value -> Some value

let parentUpdate x y =
    match x, y with
    | Option.None, Option.None -> Option.None
    | Option.None, Some value -> Option.None
    | Some value, Option.None -> Some value
    | Some value1, Some value2 -> Some value2

let parentBFS (gMtx: SparseMatrix<'value>) (startV: list<uint>) =

    let apexes = List.map (fun x -> (x, x)) startV
    let front = SparseVector(apexes, gMtx.ColumnCount)

    let arr = Array.init (Convert.ToInt32 front.Length) (fun i -> Some(uint i))
    let indexOfVector = SparseVector(arr)

    //let visited = (SparseVector(BinaryTree.None, gMtx.ColumnCount))

    let rec inner (front: SparseVector<uint>) visited =
        if front.isEmpty then
            visited
        else
            let newFront = addVector mask (multiplication add mult front gMtx) visited
            let visited = addVector plusVisited newFront visited
            inner (addVector parentUpdate newFront indexOfVector) visited

    inner front front
