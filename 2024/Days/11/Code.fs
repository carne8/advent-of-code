module AdventOfCode._2024._11
// A really interesting day...

#nowarn 40 // For the recursion
open System
open System.IO
open System.Collections.Generic

let input () =
    "input.txt"
    |> File.ReadAllText
    |> fun s -> s.Split " "
    |> Array.map int64

// --- Firstly tried the "naive" way ---
// let blink stones =
//     stones
//     |> Array.collect (fun stone ->
//         match stone with
//         | 0L -> [| 1L |]
//         | stone when (stone |> string |> String.length)%2 = 0 ->
//             let stoneStr = stone |> string

//             stoneStr
//             |> Seq.splitInto 2
//             |> Seq.map (String >> int64)
//             |> Seq.toArray
//         | stone -> [| stone * 2024L |]
//     )

// --- Secondly tried the using a linked list ---
// let updateNode (node: LinkedListNode<int64>) =
//     match node.Value with
//     | 0L -> node.Value <- 1L
//     | stone when (stone |> string |> String.length)%2 = 0 ->
//         let stoneA, stoneB =
//             stone
//             |> string
//             |> Seq.splitInto 2
//             |> Seq.map (String >> int64)
//             |> fun newStones ->
//                 newStones |> Seq.item 0,
//                 newStones |> Seq.item 1

//         node.Value <- stoneA
//         node.List.AddAfter(node, stoneB) |> ignore

//     | stone -> node.Value <- stone * 2024L

// let blink (list: LinkedList<int64>) =
//     let mutable node = list.First
//     let mutable next = list.First.Next

//     while node <> null do
//         node |> updateNode
//         node <- next
//         if node <> null then
//             next <- node.Next

// --- Finlay used recursive function with memoization ---
let memoize f =
    let dict = new Dictionary<_,_>()
    fun n ->
        match dict.TryGetValue(n) with
        | (true, v) -> v
        | _ ->
            let temp = f n
            dict.Add(n, temp)
            temp

let rec count = memoize <| fun (steps, stone) ->
    match steps with
    | 0 -> 1L
    | _ ->
        match stone with
        | 0L -> count (steps-1, 1L)
        | stone when (stone |> string |> String.length)%2 = 0 ->
            let stoneA, stoneB =
                stone
                |> string
                |> Seq.splitInto 2
                |> Seq.map (String >> int64)
                |> fun newStones ->
                    newStones |> Seq.item 0,
                    newStones |> Seq.item 1

            count (steps-1, stoneA)
            + count (steps-1, stoneB)
        | _ -> count (steps-1, stone*2024L)

let partOne = input() |> Array.sumBy (fun stone -> count(25, stone))
let partTwo = input() |> Array.sumBy (fun stone -> count(75, stone))