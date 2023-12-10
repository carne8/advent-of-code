open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "./input.txt" |> List.ofArray
let testInput = File.ReadAllLines "./test-input.txt" |> List.ofArray


let parseHistory (str: string) =
    str.Split " "
    |> Seq.toList
    |> List.choose (function
        | "" -> None
        | i -> i |> int |> Some)

let historyToDifferences (history: int list) =
    history
    |> List.pairwise
    |> List.map (fun (prev, next) -> next - prev)

let analyzeHistory (history: int list) =
    let differences = history |> List.singleton |> List

    while differences
        |> Seq.last
        |> Seq.forall ((=) 0)
        |> not
        do
        differences
        |> Seq.last
        |> historyToDifferences
        |> differences.Add

    differences |> Seq.toList


let extrapolateHistory history =
    let analyzed = history |> analyzeHistory
    let state = List()

    for line in analyzed |> List.rev do
        let prevValue =
            state
            |> Seq.tryLast
            |> Option.defaultValue 0

        (line |> List.last) + prevValue
        |> state.Add

    state |> Seq.last

let extrapolateHistoryBack history =
    let analyzed = history |> analyzeHistory
    let state = List()

    for line in analyzed |> List.rev do
        let prevValue =
            state
            |> Seq.tryLast
            |> Option.defaultValue 0

        (line |> List.head) - prevValue
        |> state.Add

    state |> Seq.last


let part1 =
    input
    |> List.map (parseHistory >> extrapolateHistory)
    |> List.sum

let part2 =
    input
    |> List.map (parseHistory >> extrapolateHistoryBack)
    |> List.sum

printfn "Part 1: %i" part1
printfn "Part 2: %i" part2