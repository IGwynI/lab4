open System

type stringBtree =
    | Node of string * stringBtree * stringBtree
    | Nil

// Вставка
let rec insert tree value =
    match tree with
    | Nil -> Node(value, Nil, Nil)
    | Node(x, left, right) ->
        if value < x then 
            Node(x, insert left value, right)
        else 
            Node(x, left, insert right value)

// Генерация случайной строки
let randomString (rnd: Random) length =
    let chars = 
        Array.concat [ [|'a'..'z'|]; [|'A'..'Z'|]; [|'0'..'9'|] ]

    let len = max 1 length

    String(Array.init len (fun _ -> 
        chars.[rnd.Next(chars.Length)]))

// Замена последнего символа
let replaceLastChar newChar (str: string) =
    if String.IsNullOrEmpty(str) then 
        str
    else 
        str.[0 .. str.Length - 2] + string newChar


let rec treeMap f tree =
    match tree with
    | Nil -> Nil
    | Node(x, left, right) ->
        Node(f x, treeMap f left, treeMap f right)

// Вывод дерева
let infix root left right = (left(); root(); right())

let iterh trav nodeAction tree =
    let rec tr tree depth =
        match tree with
        | Node(x, L, R) ->
            trav (fun () -> nodeAction x depth)
                 (fun () -> tr L (depth + 1))
                 (fun () -> tr R (depth + 1))
        | Nil -> ()
    tr tree 0

let spaces n = List.fold (fun s _ -> s + "  ") "" [0..n]

let printTree tree =
    printfn "Дерево:"
    iterh infix (fun x h -> printfn "%s%s" (spaces h) x) tree

// Ввод целого
let rec readInt prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | (true, n) when n > 0 -> n
    | _ ->
        printfn "Ошибка! Введите целое положительное число."
        readInt prompt

// Ввод символа
let rec readChar prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    if String.IsNullOrEmpty(input) || input.Length <> 1 then
        printfn "Ошибка! Введите ровно один символ."
        readChar prompt
    else
        input.[0]


[<EntryPoint>]
let main argv =
    let rnd = Random()

    let count = readInt "Введите количество строк в дереве: "


    let strings = List.init count (fun _ -> 
        randomString rnd (rnd.Next(3, 9)))

    printfn "\nСгенерированный список: %A" strings

    // Построение дерева
    let tree = List.fold insert Nil strings
    printfn "\nИсходное дерево:"
    printTree tree

    let newChar = readChar "Введите символ для замены последнего символа: "

    let newTree = treeMap (replaceLastChar newChar) tree

    printfn "\nДерево после замены последнего символа на '%c':" newChar
    printTree newTree

    0