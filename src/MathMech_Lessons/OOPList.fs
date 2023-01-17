module OOPListHW

open MyListHW

type IList<'Value> =
    interface
    end

type List<'Value>(head: 'Value, tail: IList<'Value>) =
    interface IList<'Value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'Value>() =
    interface IList<'Value>

///Функция concatenation добавляет к концу (tail) первого списка второй список
let rec сoncatenation (lst1: IList<'Value>) (lst2: IList<'Value>) =
    match lst1 with
    | :? EmptyList<'Value> -> lst2
    | :? List<'Value> as lst1 -> List(lst1.Head, сoncatenation lst1.Tail lst2)
    | _ -> failwith $"You can use EmptyList or List types"

///Функция возвращает head
let head (lst: IList<'Value>) : 'Value =
    match lst with
    | :? List<'Value> as lst -> lst.Head
    | _ -> failwith $"You can use EmptyList or List types"

///Функция возвращает tail
let tail (lst: IList<'Value>) : IList<'Value> =
    match lst with
    | :? List<'Value> as lst -> lst.Tail
    | _ -> failwith $"You can use EmptyList or List types"

///Функция возврашает длину List
let rec lenght (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> 0
    | :? List<'Value> as lst ->
        lenght lst.Tail
        + 1
    | _ -> failwith $"You can use EmptyList or List types"

///Функция преобразовывает MyList в OOPList
let rec myListToOOPList (lst: MyList<'Value>) =
    match lst with
    | Empty -> EmptyList() :> IList<'Value>
    | Cons (head, tail) -> List(head, myListToOOPList tail)

///Функция преобразовывает OOPList в MyList
let rec oopListToMyList (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> Empty
    | :? List<'Value> as lst -> Cons(lst.Head, oopListToMyList lst.Tail)
    | _ -> failwith $"You can use EmptyList or List types"

///BubbleSort
let bubbleSort (lst: IList<'Value>) =
    let rec bubble (lst: IList<'Value>) =
        match lst with
        | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
        | :? List<'Value> as lst ->
            if lst.Tail :? EmptyList<'Value> then
                lst
            elif lst.Head > head lst.Tail then
                List(head lst.Tail, bubble (List(lst.Head, tail lst.Tail)))
            else
                List(lst.Head, bubble lst.Tail)
        | _ -> failwith $"You can use EmptyList or list types"

    let mutable rez: IList<'Value> = lst

    for i in 1 .. lenght lst do
        rez <- bubble rez

    rez

///Функция получает OOPList и значение, возвращает кортеж с двумя списками
let rec separateList (lst: IList<'Value>) a =
    match lst with
    | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>, EmptyList() :> IList<'Value>
    | :? List<'Value> as lst ->
        let sorted = separateList lst.Tail a

        if
            lst.Head
            <= a
        then
            List(lst.Head, fst sorted), snd sorted
        else
            fst sorted, List(lst.Head, snd sorted)
    | _ -> failwith $"You can use EmptyList or list types"

///QuickSort
let quickSort (lst: IList<'Value>) =
    let rec quick (lst: IList<'Value>) =
        match lst with
        | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
        | :? List<'Value> as lst ->
            if lst.Tail :? EmptyList<'Value> then
                List(lst.Head, EmptyList())
            else
                let tailMinMax = separateList (List(head lst.Tail, tail lst.Tail)) lst.Head
                сoncatenation (quick (fst tailMinMax)) (List(lst.Head, quick (snd tailMinMax)))
        | _ -> failwith $"You can use EmptyList or list types"

    quick lst

let rec oopListToList lst : IList<'Value> =
    match lst with
    | [] -> EmptyList() :> IList<'Value>
    | head :: tail -> List(head, oopListToList tail)

let rec listToOOPList (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> []
    | :? List<'Value> as lst ->
        lst.Head
        :: listToOOPList lst.Tail
    | _ -> failwith $"You can use EmptyList or list types"
