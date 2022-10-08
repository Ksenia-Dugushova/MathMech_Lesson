namespace MathMech_lessons

open System.Reflection

module Main =

    //The first task. Exponentiation in a simple way
    let f1 (x: float) (y: int) : float =

        if
            x = 0.0
            && y = 0
        then
            printfn "Not defined"
            0.0
        elif x = 1.0 then
            1.0
        elif x = 0.0 then
            0.0
        else if y < 0 then
            let mutable p = 1.0

            for n in 0 .. -1 .. y do
                p <-
                    p
                    * (1.0 / x)

            p
        elif y = 0 then
            1.0
        else
            let mutable k = 1.0

            for m in 1..y do
                k <- k * x

            k


    //The second task. Exponentiation in a fast way
    let rec f2 (x: float) (y: int) : float =
        if
            x = 0.0
            && y = 0
        then
            printfn "Not defined"
            0.0
        elif x = 1.0 then
            1.0
        elif x = 0.0 then
            0.0
        else
            let k =
                if y = 0 then
                    1.0
                elif y % 2 = 1 then
                    let m = f2 x (abs y / 2)
                    m * m * x
                else
                    let m = f2 x (abs y / 2)
                    m * m

            if y > 0 then k else 1.0 / k


    //The third task. Difference between array elements
    let f3 (array: float array) =
        if array = [||] then
            failwith "Please enter several values into the array"

        elif array.Length = 1 then
            failwith "Please fill in the array with at least two elements"

        else
            let mutable max = array[0]
            let mutable min = array[0]
            //numbering from zero, so "-1"
            for i = 1 to array.Length
                         - 1 do
                if array[i] > max then
                    max <- array[i]

                if array[i] < min then
                    min <- array[i]

            max - min


    //The fourth task. Difference between array elements
    let f4 (x: int) (y: int) =
        let first_num = if x <= y then x else y
        let second_num = if x <= y then y else x

        if x % 2 = 0 then
            let result: int array = [| for i in (x + 1) .. 2 .. (y - 1) -> i |]
            result
        else
            let result: int array = [| for i in (x + 2) .. 2 .. (y - 1) -> i |]
            result

    [<EntryPoint>]
    let main (argv: string array) = 0
