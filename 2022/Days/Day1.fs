module Day1

let elvesInventories =
    System.IO.File.ReadAllText "./Inputs/Day1.fs"
    |> fun str -> str.Split "\n\n" // Get elf inventory
    |> Array.map (fun x -> x.Split "\n") // Get each item of each inventories
    |> Array.map (Array.map int) // string item -> int item
    |> Array.map Array.toList
    |> Array.toList

let elvesTotalCalories = elvesInventories |> List.map List.sum // Get total calories of each inventories

let puzzle1 =
    elvesTotalCalories
    |> List.sortDescending
    |> List.head

let puzzle2 =
    elvesTotalCalories
    |> List.sortDescending
    |> function
        | first :: second :: third :: _ -> first + second + third
        | _ -> failwith "Day 1: Puzzle 2 -> Not enough elves !"

printfn "Puzzle 1 -> %A" puzzle1
printfn "Puzzle 2 -> %A" puzzle2