module BFS

open SparseVector
open SparseMatrix
open MultiMatrix
open FSharp.Collections

let mult x y =
    match x, y with
    | Some value, Some _ -> Some value
    | _ -> Option.None

let add x y =
    match x, y with
    | Some value, _ -> Some value
    | _, Some value -> Some value
    | _ -> Option.None

let mask x y =
    match x, y with
    | Option.None, _ -> Option.None
    | Some value, Option.None -> Some value
    | Some _, _ -> Option.None

let plusVisited iter x y =
    match x, y with
    | Option.None, Option.None -> Option.None
    | Option.None, Some value -> Some value
    | Some _, Option.None -> Some iter
    | _ -> failwith $"Something wrong with SuperSum"

let BFS (graph: SparseMatrix<'value>) (apexes: list<uint>) =
    let apexes = List.map (fun x -> (x, ())) apexes
    let front = SparseVector(apexes, graph.ColumnCount)

    let visited =
        addVector (plusVisited 0u) front (SparseVector(BinaryTree.None, graph.ColumnCount))

    let rec inner (front: SparseVector<'a>) visited iter =
        if front.isEmpty then
            visited
        else
            let newFront = addVector mask (multiplication add mult front graph) visited

            let visited = addVector (plusVisited iter) newFront visited
            inner newFront visited (iter + 1u)

    inner front visited 1u
