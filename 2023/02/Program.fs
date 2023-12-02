open System
open System.IO
open System.Text.RegularExpressions

let testInput = File.ReadAllLines "./test-input.txt"
let input = File.ReadAllLines "./input.txt"

type CubeSet =
    { Red: int; Green: int; Blue: int }

    // Take "1 red, 2 green, 6 blue" and parse it
    static member parse (str: string) =
        str.Split ", "
        |> Array.fold
            (fun set newCubesColor ->
                match newCubesColor.Split " " with
                | [| amountOfCubes; "red" |] -> { set with Red = int amountOfCubes }
                | [| amountOfCubes; "green" |] -> { set with Green = int amountOfCubes }
                | [| amountOfCubes; "blue" |] -> { set with Blue = int amountOfCubes }
                | _ -> failwith "Failed to parse part of set"
            )
            { Red = 0; Green = 0; Blue = 0 }

    static member validate (maxRed, maxGreen, maxBlue) set =
        set.Red <= maxRed
        && set.Green <= maxGreen
        && set.Blue <= maxBlue

type Game =
    { Id: int
      Sets: CubeSet [] }

    static member private gameIdRegex = Regex("Game (\d+): (.+)")
    static member private getGameId =
        Game.gameIdRegex.Match
        >> fun m -> int m.Groups[1].Value, m.Groups[2].Value

    // Sample: "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    static member parse (str: string) =
        let gameId, restOfStr = str |> Game.getGameId
        let sets = restOfStr.Split("; ") |> Array.map CubeSet.parse

        { Id = gameId
          Sets = sets }

    static member private findMinimumCubesNeeded (game: Game) =
        game.Sets
        |> Array.fold
            (fun minimums set ->
                let minRed, minGreen, minBlue = minimums

                Math.Max(set.Red, minRed),
                Math.Max(set.Green, minGreen),
                Math.Max(set.Blue, minBlue)
            )
            (0, 0, 0)

    static member getPower =
        Game.findMinimumCubesNeeded
        >> fun (x, y, z) -> x * y * z

let part1 =
    let maxes = 12, 13, 14
    let isSetValid = CubeSet.validate maxes

    input
    |> Array.map Game.parse
    |> Array.filter (_.Sets >> Array.forall isSetValid)
    |> Array.sumBy _.Id

let part2 =
    input
    |> Array.map (Game.parse >> Game.getPower)
    |> Array.sum

printfn "Part 1: %A" part1
printfn "Part 2: %A" part2