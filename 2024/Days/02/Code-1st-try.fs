module AdventOfCode._2024._02

open System
open System.IO

let input =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (String.split " " >> Array.map int)

// let isReportSafe (report: int array) =
//     let diffsAreValid =
//         report
//         |> Array.pairwise
//         |> Array.forall (fun (a, b) ->
//             let diff = (a - b) |> Math.Abs
//             1 <= diff && diff <= 3
//         )

//     let sameVariation () =
//         let mutable isOk = true
//         let mutable ascending = None
//         let mutable prev = report[0]
//         let mutable i = 0

//         while i < report.Length - 1 && isOk do
//             i <- i + 1
//             let next = report[i]

//             match ascending with
//             | None ->
//                 match Math.Sign(next - prev) with
//                 | -1 -> ascending <- Some false
//                 | 0 -> ascending <- None
//                 | 1 -> ascending <- Some true
//                 | _ -> failwith "A"
//             | Some true -> isOk <- prev <= next
//             | Some false -> isOk <- prev >= next

//             prev <- next

//         isOk

//     diffsAreValid && sameVariation ()

let isReportSafe (report: int array) =
    let diffs =
        report
        |> Array.pairwise
        |> Array.map (fun (a, b) -> b - a)

    let mutable variation = diffs |> Array.head |> Math.Sign

    let sameVariation = diffs |> Array.forall (Math.Sign >> (=) variation)
    let correctVariation () =
        diffs |> Array.forall (
            Math.Abs >>
            fun diff -> 1 <= diff && diff <= 3
        )

    sameVariation && correctVariation()

let isReportSafe2 (report: int array) =
    let diffs =
        report
        |> Array.pairwise
        |> Array.mapi (fun idx (a, b) -> {| Idx = idx; Diff = b - a |})

    let mutable variation =
        diffs
        |> Array.groupBy (_.Diff >> Math.Sign)
        |> Array.maxBy (snd >> Array.length)
        |> fst

    let incorrectVariationDirections =
        diffs
        |> Array.filter (_.Diff >> Math.Sign >> (<>) variation)

    let incorrectVariations =
        diffs |> Array.filter (
            _.Diff
            >> Math.Abs
            >> fun diff -> 1 <= diff && diff <= 3
            >> not
        )

    // printfn ""
    // printfn "Report: %A" report
    // printfn "diffs: %A" (report |> Array.pairwise)
    printfn "diffs: %A" diffs
    // printfn "var: %A" (diffs |> Array.groupBy (_.Diff >> Math.Sign))
    // printfn "%A" incorrectVariationDirections
    // printfn "%A" incorrectVariations

    let retryErroredReport idxToRemove report =
        let mutable xxx = null

        report
        |> Array.indexed
        |> Array.choose (fun (idx, el) ->
            match idx <> idxToRemove with
            | true -> Some el
            | false -> None
        )
        |> fun x -> xxx<-x; x
        |> isReportSafe
        // |> fun b ->
        //     // if b = false then
        //     printfn "\nFIXED REPORT:\n%A\n%A" report xxx
        //     b

    match incorrectVariationDirections, incorrectVariations with
    | [||], [||] -> true
    | [| x |], [||]
    | [||], [| x |] ->
        report |> retryErroredReport x.Idx || report |> retryErroredReport (x.Idx + 1)
    | [| x |], [| y |] when x.Idx = y.Idx ->
        report |> retryErroredReport x.Idx || report |> retryErroredReport (x.Idx + 1)
    | _ -> false

let resPartOne =
    input
    |> Array.filter isReportSafe
    |> Array.length

let resPartTwo =
    input
    |> Array.filter isReportSafe2
    |> Array.length
