module Day4

let arrayToTuple (arr: _ []) = arr.[0], arr.[1]

let input = System.IO.File.ReadAllLines "./Inputs/Day4.txt"
let parsedInput =
    input
    |> List.ofArray
    |> List.map (fun str -> str.Split ",") // Split pairs
    |> List.map arrayToTuple // Split pair into tuple of sections
    |> List.map (fun (sec1, sec2) ->
        sec1.Split "-" |> Array.map int |> arrayToTuple,
        sec2.Split "-" |> Array.map int |> arrayToTuple
    )


let puzzle1 =
    parsedInput
    |> List.filter (fun (sec1, sec2) ->
        let sec1min, sec1max = sec1
        let sec2min, sec2max = sec2

        sec1min >= sec2min && sec1max <= sec2max
        || sec2min >= sec1min && sec2max <= sec1max
    )
    |> List.length

let puzzle2 =
    parsedInput
    |> List.filter (fun (sec1, sec2) ->
        let sec1min, sec1max = sec1
        let sec2min, sec2max = sec2

        sec1max >= sec2min && sec1min <= sec2max
    )
    |> List.length

printfn "Puzzle 1 -> %A" puzzle1
printfn "Puzzle 2 -> %A" puzzle2