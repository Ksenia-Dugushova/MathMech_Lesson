namespace MathMech_lessons

open System.Reflection

module Main =

//The first task. Bubble sorting
let bubbleSort array =
    let rec f (array : a[]) =
        let mutable max = 0
        for i = 0 to array.Length - 2 do
            if array[i] > array[i+1] then
                max i (i+1) array
                max <- max + 1

        if max > 0 then loop array else array

    loop array
    [<EntryPoint>]
    let main (argv: string array) = 0
