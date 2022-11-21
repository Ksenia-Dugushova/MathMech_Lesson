module OOPListHW

open MyListHW

type OOPList<'value> =
    interface
    end

type List<'value>(head: 'value, tail: OOPList<'value>) =
    interface OOPList<'value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'value>() =
    interface OOPList<'value>

///Функция concatenation добавляет к концу (tail) первого списка второй список
let rec сoncatenation (lst1: OOPList<'value>) (lst2: OOPList<'value>) =
    match lst1 with
    | :? EmptyList<'value> -> lst2
    | :? List<'value> as lst1 -> List(lst1.Head, сoncatenation lst1.Tail lst2)
    | _ -> failwith $"You can use EmptyList or List types"

///Функция возвращает head
let head (lst: OOPList<'value>) : 'value =
    match lst with
    | :? List<'value> as lst -> lst.Head
    | _ -> failwith $"You can use EmptyList or List types"

///Функция возвращает tail
let tail (lst: OOPList<'value>) : OOPList<'value> =
    match lst with
    | :? List<'value> as lst -> lst.Tail
    | _ -> failwith $"You can use EmptyList or List types"

///Функция возврашает длину List
let rec lenght (lst: OOPList<'value>) =
    match lst with
    | :? EmptyList<'value> -> 0
    | :? List<'value> as lst ->
        lenght lst.Tail
        + 1
    | _ -> failwith $"You can use EmptyList or List types"

///Функция преобразовывает MyList в OOPList
let rec myList_OOPList (lst: MyList<'value>) =
    match lst with
    | Empty -> EmptyList() :> OOPList<'value>
    | Cons (head, tail) -> List(head, myList_OOPList tail)

///Функция преобразовывает OOPList в MyList
let rec OOPList_MyList (lst: OOPList<'value>) =
    match lst with
    | :? EmptyList<'value> -> Empty
    | :? List<'value> as lst -> Cons(lst.Head, OOPList_MyList lst.Tail)
    | _ -> failwith $"You can use EmptyList or List types"

///BubbleSort
let bubbleSort (lst: OOPList<'value>) =
    let rec bubble (lst: OOPList<'value>) =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> OOPList<'value>
        | :? List<'value> as lst ->
            if lst.Tail :? EmptyList<'value> then
                lst
            elif lst.Head > head lst.Tail then
                List(head lst.Tail, bubble (List(lst.Head, tail lst.Tail)))
            else
                List(lst.Head, bubble lst.Tail)
        | _ -> failwith $"You can use EmptyList or list types"

    let mutable rez: OOPList<'value> = lst

    for i in 1 .. lenght lst do
        rez <- bubble rez

    rez

///Функция получает OOPList и значение, возвращает кортеж с двумя списками
let rec separateList (lst: OOPList<'value>) a =
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> OOPList<'value>, EmptyList() :> OOPList<'value>
    | :? List<'value> as lst ->
        let sorted = separateList lst.Tail a
        if lst.Head <= a then
            List(lst.Head, fst sorted), snd sorted
        else
            fst sorted, List(lst.Head, snd sorted)
    | _ -> failwith $"You can use EmptyList or list types"

///QuickSort
let quickSort (lst: OOPList<'value>) =
    let rec quick (lst: OOPList<'value>) =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> OOPList<'value>
        | :? List<'value> as lst ->
            if lst.Tail :? EmptyList<'value> then
                List(lst.Head, EmptyList())
            else
                let tailMinMax = separateList (List(head lst.Tail, tail lst.Tail)) lst.Head
                сoncatenation (quick(fst tailMinMax)) (List(lst.Head, quick(snd tailMinMax)))
        | _ -> failwith $"You can use EmptyList or list types"

    quick lst

let rec OOPListToList lst : OOPList<'value> =
    match lst with
    | [] -> EmptyList() :> OOPList<'value>
    | head :: tail -> List(head, OOPListToList tail)

let rec listToOOPList (lst: OOPList<'value>) =
    match lst with
    | :? EmptyList<'value> -> []
    | :? List<'value> as lst -> lst.Head :: listToOOPList lst.Tail
    | _ ->
        failwith
            $"You can use EmptyList or list types"
