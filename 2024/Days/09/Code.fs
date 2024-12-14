module AdventOfCode._2024._09

open System.IO
open System.Collections.Generic

let getDiskMap () =
    "input.txt"
    |> File.ReadAllText
    |> fun str -> str.ToCharArray()
    |> Array.map (string >> int)

type Block =
    { Value: int option
      Length: int }

let sortBlocks blocks =
    let mutable i = 0
    let mutable revI = blocks |> Array.length |> (+) -1
    let mutable sortedBlocks = blocks |> Array.copy

    while i <> revI do
        let block = blocks |> Array.item i
        match block with
        | None ->
            let mutable revChar = blocks |> Seq.item revI
            while revChar |> Option.isNone do
                revI <- revI-1
                revChar <- blocks |> Seq.item revI

            Array.set sortedBlocks i revChar
            Array.set sortedBlocks revI None

            revI <- revI-1
        | Some _fileId -> ()

        i<-i+1

    sortedBlocks

let sortBlocksPartTwo (blocks: Block array) =
    let sortedBlocks = blocks |> LinkedList
    let mutable finished = false
    let mutable lastMovedBlockValue: int option = None

    while not finished do
        let blockToMove = // Get last non empty block
            let mutable foundBlock = None
            let mutable currentBlock = sortedBlocks.Last

            while foundBlock |> Option.isNone && currentBlock <> null do
                match currentBlock.Value.Value, lastMovedBlockValue with
                | Some _, None -> foundBlock <- Some currentBlock
                | Some v, Some lastMovedValue when v < lastMovedValue ->
                    foundBlock <- Some currentBlock
                | _ -> currentBlock <- currentBlock.Previous

            foundBlock

        match blockToMove with
        | None -> finished <- true
        | Some blockToMove ->
            // let blockToMove = blockToMove.Value
            lastMovedBlockValue <- Some blockToMove.Value.Value.Value

            let emptySpace = // Get first sufficiently long empty space
                let mutable foundBlock = None
                let mutable currentBlock = sortedBlocks.First

                while
                    foundBlock |> Option.isNone
                    && currentBlock <> null // Check if we are not at the end of the list
                    && currentBlock.Value <> blockToMove.Value // Exit when we reach the block to move (we don't want to move the block towards the end of the list)
                    do
                    match currentBlock.Value.Value with
                    | None when currentBlock.Value.Length >= blockToMove.Value.Length ->
                        foundBlock <- Some currentBlock
                    | _ -> currentBlock <- currentBlock.Next

                foundBlock

            match emptySpace with
            | None -> ()
            | Some emptySpace ->
                // let emptySpace = emptySpace.Value
                let newEmptySpaceSize = emptySpace.Value.Length - blockToMove.Value.Length

                // Add empty block where we remove our block
                sortedBlocks.AddAfter(
                    blockToMove,
                    { Value = None; Length = blockToMove.Value.Length }
                ) |> ignore

                // Move our block
                sortedBlocks.Remove blockToMove
                sortedBlocks.AddBefore(emptySpace, blockToMove) |> ignore

                // Ensure we did not removed unused empty space
                if newEmptySpaceSize > 0 then
                    sortedBlocks.AddBefore(
                        emptySpace,
                        { Value = None; Length = newEmptySpaceSize }
                    ) |> ignore

                // Remove the filled empty space
                sortedBlocks.Remove emptySpace

    sortedBlocks
    |> Seq.toArray

let calculateChecksum blocks =
    blocks
    |> Array.indexed
    |> Array.fold
        (fun (acc: int64) (idx, block) ->
            match block with
            | None -> acc
            | Some fileId -> acc + (fileId*idx |> int64))
        0

let calculateChecksumPartTwo blocks =
    blocks
    |> Array.collect (fun block ->
        Array.create block.Length { Value = block.Value; Length = 1 }
    )
    |> Array.indexed
    |> Array.fold
        (fun (acc: int64) (idx, block) ->
            match block.Value with
            | None -> acc
            | Some fileId -> acc + (fileId*idx |> int64))
        0

let partOne =
    let diskMap = getDiskMap()
    let blocks =
        let mutable fileId = 0

        diskMap
        |> Array.indexed
        |> Array.collect (fun (idx, number) ->
            match idx%2 = 0 with
            | true -> // File
                let block = Array.create number (Some fileId)
                fileId <- fileId + 1
                block
            | false -> Array.create number None
        )

    // let visualize blocks =
    //     blocks
    //     |> Array.map (function
    //         | Some fileId -> fileId |> string
    //         | None -> "."
    //     )
    //     |> String.concat ""

    blocks
    |> sortBlocks
    |> calculateChecksum

let partTwo =
    let diskMap = getDiskMap()
    let blocks =
        let mutable fileId = 0

        diskMap
        |> Array.indexed
        |> Array.map (fun (idx, number) ->
            match idx%2 = 0 with
            | true -> // File
                let block = { Value = Some fileId; Length = number }
                fileId <- fileId+1
                block
            | false -> { Value = None; Length = number }
        )

    // let visualize blocks =
    //     blocks
    //     |> Array.map (fun block ->
    //         match block.Value with
    //         | Some fileId -> fileId |> string
    //         | None -> "."
    //         |> String.replicate block.Length
    //     )
    //     |> String.concat ""

    blocks
    |> sortBlocksPartTwo
    |> calculateChecksumPartTwo