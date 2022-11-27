module MyListHW

type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty

///Функция concatenation добавляет к концу (tail) первого списка второй список
let rec сoncatenation (lst1: MyList<'value>) (lst2: MyList<'value>) =
    match lst1 with
    | Empty -> lst2
    | Cons (head, tail) -> Cons(head, сoncatenation tail lst2)

let rec lenght (lst: MyList<'value>) =
    match lst with
    | Empty -> 0
    | Cons (_, tail) ->
        lenght tail
        + 1

///Bubble Sort
let bubbleSort (lst: MyList<'value>) =
    let rec bubble (lst: MyList<'value>) =
        match lst with
        | Empty -> Empty
        | Cons (_, Empty) -> lst
        | Cons (head1, Cons (head2, tail)) ->
            Cons(min head1 head2, bubble (Cons(max head1 head2, tail)))

    let mutable rez: MyList<'value> = lst

    for i in 1 .. lenght lst do
        rez <- bubble rez

    rez

///Функция получает MyList и значение, возвращает кортеж с двумя списками, сравнивая элементы листа с полученным значением
let rec separationList (lst: MyList<'value>) a =
    match lst with
    | Empty -> Empty, Empty
    | Cons (head, tail) ->
        let part = separationList tail a

        if head <= a then
            Cons(head, fst part), snd part
        else
            fst part, Cons(head, snd part)

///QuickSort
let quickSort (lst: MyList<'value>) =
    let rec quick (lst: MyList<'value>) =
        match lst with
        | Empty -> Empty
        | Cons (head, Empty) -> Cons(head, Empty)
        | Cons (head, tail) ->
            let part = separationList tail head
            сoncatenation (quick (fst part)) (Cons(head, quick (snd part)))

    quick lst
