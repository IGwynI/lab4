open System

type IntTree =
    | Node of int * IntTree * IntTree
    | Nil

// Вставка
let rec insert tree value =
    match tree with
    | Nil -> Node(value, Nil, Nil)
    | Node(x, left, right) ->
        if value < x then Node(x, insert left value, right)
        else Node(x, left, insert right value)


let randomInt (rnd: Random) = rnd.Next(-100, 101)


let rec foldTree fNode fLeaf tree =
    match tree with
    | Nil -> fLeaf
    | Node(x, left, right) ->
        fNode x (foldTree fNode fLeaf left) (foldTree fNode fLeaf right)

// Сумма четных листьев
let sumEvenLeaves tree =
    let fLeaf = (0, true)
    let fNode x (lSum, lIsNil) (rSum, rIsNil) =
        let sum = lSum + rSum
        let isLeaf = lIsNil && rIsNil
        let newSum = 
            if isLeaf && x % 2 = 0 then 
                sum + x 
            else 
                sum
        (newSum, false)
    let (result, _) = foldTree fNode fLeaf tree
    result

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

let spaces n = String.replicate n "  "

let printTree tree =
    printfn "Дерево:"
    iterh infix (fun x h -> printfn "%s%d" (spaces h) x) tree

// Ввод целого
let rec readInt prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | (true, n) when n > 0 -> n
    | _ ->
        printfn "Ошибка! Введите целое положительное число."
        readInt prompt

[<EntryPoint>]
let main argv =
    let rnd = Random()

    let count = readInt "Введите количество элементов в дереве: "

    let numbers = List.init count (fun _ -> randomInt rnd)
    printfn "\nСгенерированный список: %A" numbers

    let tree = List.fold insert Nil numbers
    printfn "\nИсходное дерево:"
    printTree tree


    let sum = sumEvenLeaves tree
    printfn "\nСумма четных значений в листьях: %d" sum

    0