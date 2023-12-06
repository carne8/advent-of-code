open System
open System.IO

let input = File.ReadAllLines "./input.txt"

type Race =
    { Duration: int64
      RecordDistance: int64 }

    // Time to wait = (race.Duration - heldTime) * heldTime

    static member race holdTime race : int64 = (race.Duration - holdTime) * holdTime

let parseNumberRow (str: string) =
    str.Split " "
    |> Array.filter ((<>) "")
    |> Array.map int

let part1 =
    let times =
        let line = input |> Array.item 0
        line.Substring "Time:".Length
        |> parseNumberRow

    let distances =
        let line = input |> Array.item 1
        line.Substring "Distance:".Length
        |> parseNumberRow

    let races =
        [| times; distances |]
        |> Array.transpose
        |> Array.map (function
            | [| fst; snd |] -> { Duration = fst; RecordDistance = snd }
            | other -> failwithf "Failed to parse to race: %A" other)

    races
    |> Array.map (fun race ->
        Array.init
            (int race.Duration + 1)
            (fun holdTime -> race |> Race.race holdTime)
        |> Array.filter (fun raceDistance -> raceDistance > race.RecordDistance)
        |> Array.length
    )
    |> Array.reduce (*)

let part2 =
    let time =
        let line = input |> Array.item 0
        let numbers = line.Substring "Time:".Length
        numbers.ToCharArray()
        |> Array.filter Char.IsDigit
        |> String
        |> int

    let distance =
        let line = input |> Array.item 1
        let numbers = line.Substring "Distance:".Length
        numbers.ToCharArray()
        |> Array.filter Char.IsDigit
        |> String
        |> int64

    let race = { Duration = time; RecordDistance = distance }

    let mutable waysToWin = 0

    for holdTime = 0 to int race.Duration + 1 do
        let racedDistance = race |> Race.race holdTime

        match racedDistance > race.RecordDistance with
        | true -> waysToWin <- waysToWin + 1
        | false -> ()

    waysToWin


printfn "Part 1: %i" part1
printfn "Part 2: %i" part2