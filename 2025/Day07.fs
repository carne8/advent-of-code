module AOC._2025.Day07

open System.IO
open System.Collections.Generic

let input =
    let lines = "input.txt" |> File.ReadAllLines
    let width = lines[0].Length
    let height = lines.Length

    let isSplitterMap =
        Array.init
            height
            (fun _ -> Array.create width false)
    let mutable startPos = ValueNone

    lines |> Array.iteri (fun lineIdx line ->
        line |> Seq.iteri (fun columnIdx char ->
            isSplitterMap[lineIdx][columnIdx] <- char = '^'

            if char = 'S' then
                startPos <- ValueSome (lineIdx, columnIdx)
        )
    )

    startPos.Value, isSplitterMap

let extendBeams beams (splitterLine: bool array) =
    let mutable splits = 0

    beams
    |> Array.collect (fun idx ->
        if splitterLine[idx] then
            splits <- splits + 1
            [| idx-1; idx+1 |]
        else
            [| idx |]
    )
    |> Array.distinct,
    splits

let partOne () =
    let start, isSplitterMap = input

    let mutable beams = [| snd start |]
    let mutable splits = 0

    for lineIdx = 2 to Array.length isSplitterMap - 1 do
        let splitters = isSplitterMap[lineIdx]
        let newBeams, newSplits =
            extendBeams
                beams
                splitters

        beams <- newBeams
        splits <- splits + newSplits

    splits

let memo = new Dictionary<_, _>()
let rec countTimelines line beamColumn (splitters: bool array array) =
    if line = splitters.Length then 1L else

    match memo.TryGetValue((line, beamColumn)) with
    | true, res -> res
    | false, _ ->
        let res =
            if splitters[line][beamColumn] then
                countTimelines (line+1) (beamColumn + 1) splitters
                + countTimelines (line+1) (beamColumn - 1) splitters
            else
                countTimelines (line+1) beamColumn splitters

        memo.Add((line, beamColumn), res)
        res

let partTwo () =
    let start, splitters = input
    countTimelines 2 (snd start) splitters