module AdventOfCode._2024._22

open System
open System.IO

let getInput () =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map int
    |> Array.toList

module SecretNumber =
    let getNext (secretNumber: int64) =
        let (_, a) = Math.DivRem((secretNumber <<< 6) ^^^ secretNumber, 16777216L)
        let (_, b) = Math.DivRem((a >>> 5) ^^^ a, 16777216L)
        let (_, c) = Math.DivRem((b <<< 11) ^^^ b, 16777216L)
        c

    let rec generateNth n secretNumber =
        match n with
        | 0 -> secretNumber
        | _ ->
            secretNumber
            |> getNext
            |> generateNth (n-1)

let partOne () =
    getInput()
    |> List.map int64
    |> List.sumBy (SecretNumber.generateNth 2000)

let partTwo () = // Slow, but it works
    getInput()
    |> List.map int64
    |> List.collect (fun secretNumber ->
        // Buyer price changes
        let changes =
            let mutable secretNumber = secretNumber
            [ for _ in 0..2000 do
                yield secretNumber |> string |> Seq.last |> string |> int // Get last digit
                secretNumber <- secretNumber |> SecretNumber.getNext ]
            |> List.pairwise
            |> List.map (fun (a, b) ->
                b, b - a
            )

        // Trigger sequences
        changes
        |> List.windowed 4
        |> List.map (fun window ->
            window |> List.map snd, // Window's changes
            window |> List.last |> fst // Window's sell price
        )
        |> List.distinctBy fst // A trigger sequence can only be used once for each buyer
    )
    |> List.groupBy fst
    |> List.map (fun (window, l) -> window, l |> List.sumBy snd) // Window with sell prices
    |> List.maxBy snd // Best changes sequence
    |> snd