module AdventOfCode._2024._13

open System.IO
open System.Text.RegularExpressions

let private mapTuple f t = t |> fst |> f, t |> snd |> f

type ClawMachine =
    { PrizeCoordinates: int * int
      ButtonA: int * int
      ButtonB: int * int }

    static member tryFindCombination (cm: ClawMachine) =
        let a1, a2 = cm.ButtonA
        let b1, b2 = cm.ButtonB
        let c1, c2 = cm.PrizeCoordinates

        // Use the Cramer's rule to find
        // the solutions for the system:
        // a1*x + b1*y = c1
        // a2*x + b2*y = c2

        let det = a1*b2 - b1*a2
        match det with
        | 0 -> None
        | _ ->
            let x = float (c1*b2-b1*c2) / float det
            let y = float (a1*c2-c1*a2) / float det

            match x >= 0 && y >= 0 with
            | false -> None
            | true ->
                match x % 1., y % 1. with
                | 0., 0. -> Some (int x, int y)
                | _ -> None

type ClawMachine64 =
    { PrizeCoordinates: int64 * int64
      ButtonA: int * int
      ButtonB: int * int }

    static member fromClawMachine (cm: ClawMachine) =
        let prizeOffset = 10000000000000L
        { PrizeCoordinates =
            (prizeOffset + (cm.PrizeCoordinates |> fst |> int64),
             prizeOffset + (cm.PrizeCoordinates |> snd |> int64))
          ButtonA = cm.ButtonA
          ButtonB = cm.ButtonB }

    static member tryFindCombination cm =
        let a1, a2 = cm.ButtonA |> mapTuple int64
        let b1, b2 = cm.ButtonB |> mapTuple int64
        let c1, c2 = cm.PrizeCoordinates

        // Get solutions for the system
        // a1*x + b1*y = c1
        // a2*x + b2*y = c2

        let det = a1*b2 - b1*a2
        match det with
        | 0L -> None
        | _ ->
            let x = float (c1*b2-b1*c2) / float det
            let y = float (a1*c2-c1*a2) / float det

            match x >= 0 && y >= 0 with
            | false -> None
            | true ->
                match x % 1., y % 1. with
                | 0., 0. -> Some (int64 x, int64 y)
                | _ -> None

let input () : ClawMachine array =
    let regex = Regex @"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)"

    "input.txt"
    |> File.ReadAllText
    |> fun text -> text.Split "\n\n"
    |> Array.map (regex.Match >> fun m ->
        { ButtonA = int m.Groups[1].Value, int m.Groups[2].Value
          ButtonB = int m.Groups[3].Value, int m.Groups[4].Value
          PrizeCoordinates = int m.Groups[5].Value, int m.Groups[6].Value }
    )

let buttonAPrize = 3
let buttonBPrize = 1

let partOne =
    input () |> Array.sumBy (
        ClawMachine.tryFindCombination
        >> function
        | None -> 0
        | Some (a, b) ->
            a * buttonAPrize
            + b * buttonBPrize
    )

let partTwo =
    input ()
    |> Array.map ClawMachine64.fromClawMachine
    |> Array.fold
        (fun acc cm ->
            cm
            |> ClawMachine64.tryFindCombination
            |> function
            | None -> acc
            | Some (a, b) ->
                a*(int64 buttonAPrize) + b*(int64 buttonBPrize)
                |> (+) acc
        )
        0L