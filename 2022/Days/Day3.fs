module Day3

open System.IO

let input =
    "./Inputs/Day3.txt"
    |> File.ReadAllLines

let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let getItemPriority (char: char) = char |> alphabet.IndexOf |> (+) 1

let parsedInput =
    input
    |> Seq.map (fun rucksack -> rucksack |> Seq.splitInto 2)
    |> Seq.map (fun rucksack ->
        rucksack |> Seq.item 0 |> Set.ofArray,
        rucksack |> Seq.item 1 |> Set.ofArray
    )

let puzzle1 =
    parsedInput
    |> Seq.map (fun (comp1, comp2) ->
        Set.intersect comp1 comp2
        |> Seq.head
    )
    |> Seq.map getItemPriority
    |> Seq.sum

let puzzle2 =
    parsedInput
    |> Seq.chunkBySize 3
    |> Seq.map (fun rucksacks ->
        rucksacks
        |> Seq.map (fun (x, y) -> x + y)
        |> Set.intersectMany
        |> Seq.head
    )
    |> Seq.map getItemPriority
    |> Seq.sum

printfn "Puzzle 1 -> %A" puzzle1
printfn "Puzzle 2 -> %A" puzzle2