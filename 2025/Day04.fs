module AOC._2025.Day04

open System
open System.IO

[<Struct>]
type Case =
    | Roll
    | Empty

let input =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (
        Seq.map (function
            | '@' -> Roll
            | _ -> Empty
        )
    )
    |> array2D

let relativeAdjacentPositions =
    [| struct (-1, -1); struct (0, -1); struct (1, -1)
       struct (-1, 0); struct (1, 0)
       struct (-1, 1); struct (0, 1); struct (1, 1) |]

let isRollAccessible (diagram: _ array2d) struct (a, b) =
    relativeAdjacentPositions
    |> Array.filter (fun struct (x, y) ->
        let x'= a+x
        let y'= b+y

        0 <= x' && x' <= (diagram |> Array2D.length1) - 1
        && 0 <= y' && y' <= (diagram |> Array2D.length2) - 1
        && diagram[x', y'] = Roll
    )
    |> Array.length
    |> fun n -> n < 4

let printDiagram (diagram: _ array2d) =
    for x = 0 to 9 do
        for y = 0 to 9 do
            match diagram[x, y] with
            | Roll when isRollAccessible diagram struct (x, y) -> printf "x"
            | Roll -> printf "@"
            | Empty -> printf "."

        printfn ""

let partOne () =
    let mutable count = 0

    for x = 0 to (input |> Array2D.length1) - 1 do
        for y = 0 to (input |> Array2D.length2) - 1 do
            match input[x, y] with
            | Roll when isRollAccessible input struct (x, y) -> count <- count + 1
            | _ -> ()

    count

let partTwo () =
    let mutable count = 0
    let mutable finished = false

    while not finished do
        let mutable removedARoll = false
        for x = 0 to (input |> Array2D.length1) - 1 do
            for y = 0 to (input |> Array2D.length2) - 1 do
                match input[x, y] with
                | Roll when isRollAccessible input struct (x, y) ->
                    count <- count + 1
                    input[x, y] <- Empty
                    removedARoll <- true
                | _ -> ()

        finished <- not removedARoll

    count