module AdventOfCode._2024._01

open System
open System.IO

let input =
    "input.txt"
    |> File.ReadAllLines
    |> Array.choose (
        String.split " "
        >> function
        | [|a; b|] -> (a |> int, b |> int) |> Some
        | _ -> None
    )
    |> Array.unzip

let list1, list2 =
    input |> fst |> Array.sort,
    input |> snd |> Array.sort

let resPartOne =
    Array.zip
        list1
        list2
    |> Array.map (fun (a, b) -> a-b |> Math.Abs)
    |> Array.sum

let resPartTwo =
    list1
    |> Array.fold
        (fun acc a ->
            let appearancesInSecondList =
                list2
                |> Array.filter ((=) a)
                |> Array.length

            printfn "%A %i %A" a appearancesInSecondList (a * appearancesInSecondList)

            acc + a * appearancesInSecondList
        )
        0