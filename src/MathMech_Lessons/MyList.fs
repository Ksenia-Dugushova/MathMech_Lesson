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
let BubbleSort (lst: MyList<'value>) =
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

///
let rec sortList (lst: MyList<'value>) a =
    match lst with
    | Empty -> Empty, Empty
    | Cons (head, tail) ->
        let sorted = sortList tail a

        if head <= a then
            Cons(head, fst sorted), snd sorted
        else
            fst sorted, Cons(head, snd sorted)

///QuickSort
let QuickSort (lst: MyList<'value>) =
    let rec Quick (lst: MyList<'value>) =
        match lst with
        | Empty -> Empty
        | Cons (head, Empty) -> Cons(head, Empty)
        | Cons (head, tail) ->
            let sorted = sortList tail head
            сoncatenation (Quick(fst sorted)) (Cons(head, Quick(snd sorted)))

    Quick lst
