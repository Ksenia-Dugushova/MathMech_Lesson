module TreesHW

open System.Collections.Generic
open MyListHW

type SomeTree<'value> =
    | Leaf of value: 'value
    | Node of parents: 'value * nodes: array<SomeTree<'value>>

let rec foldTree stacker acc tree =
    match tree with
    | Leaf value -> stacker acc value
    | Node (value, children) -> Array.fold (foldTree stacker) (stacker acc value) children

let uniqueElements tree =
    let hashSet = HashSet<'value>()

    let hashSetTree (hashSet: HashSet<'value>) value =
        hashSet.Add value
        |> ignore

        hashSet

    let res = foldTree hashSetTree hashSet tree
    res.Count

let myListTree tree =
    let treeToList list element = Cons(element, list)
    foldTree treeToList Empty tree
