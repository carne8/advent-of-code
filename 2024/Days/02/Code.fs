module AdventOfCode._2024._02

open System
open System.IO

let reports () =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (fun str -> str.Split " " |> Array.map int)

let isReportSafe (report: int array) =
    let variations =
        report
        |> Array.pairwise
        |> Array.map (fun (a, b) -> b-a)

    // All variations have the same size
    let sameSign =
        variations
        |> Array.map Math.Sign
        |> Array.distinct
        |> Array.length
        |> (=) 1

    // All variations are between 1 and 3
    let correctDiffs =
        variations |> Array.forall (fun d ->
            let absValue = Math.Abs d
            1 <= absValue && absValue <= 3
        )

    sameSign && correctDiffs

let isReportApproximatelySafe (report: int array) =
    // If the report isn't safe, try to remove
    // each value and check if it is safe

    report |> isReportSafe
    ||
    report
    |> Array.indexed
    |> Array.tryFind (fun (idx, _) ->
        report
        |> Array.indexed
        |> Array.filter (fst >> (<>) idx) // Same report, but without one value
        |> Array.map snd
        |> isReportSafe
    )
    |> Option.isSome

let partOne () =
    reports ()
    |> Array.filter isReportSafe
    |> Array.length

let partTwo () =
    reports ()
    |> Array.filter isReportApproximatelySafe
    |> Array.length