module AdventOfCode._2024._10

open System.IO

let heightMap =
    "input.txt"
    |> File.ReadAllLines
    |> Array.mapi (fun lineIdx line ->
        line.ToCharArray()
        |> Array.indexed
        |> Array.choose (fun (columnIdx, char) ->
            let height =
                try
                    char |> string |> int |> Some
                with _ -> None

            height |> Option.map (fun height ->
                (lineIdx, columnIdx, height)
            )
        )
    )

let nextPositions (lineIdx, columnIdx, height) =
    [| lineIdx, columnIdx + 1
       lineIdx, columnIdx - 1
       lineIdx + 1, columnIdx
       lineIdx - 1, columnIdx |]
    |> Array.choose (fun (lineIdx, columnIdx) ->
        heightMap
        |> Array.tryItem lineIdx
        |> Option.bind (Array.tryItem columnIdx)
        |> Option.bind (fun (y, x, h) ->
            match h = height+1 with
            | true -> Some (y, x, h)
            | false -> None
        )
    )

let getAllTrailsFromTrailhead trailhead =
    let mutable trails = [| [| trailhead |] |]

    // Do 9 iterations
    for _ in 0..8 do
        let mutable newTrails = Array.empty
        for trail in trails do
            newTrails <-
                trail
                |> Array.last
                |> nextPositions
                |> Array.map (Array.singleton >> Array.append trail)
                |> Array.append newTrails
        trails <- newTrails

    trails

let show (heightMap: (_ * _ * int) array array) =
    heightMap
    |> Array.map (
        Array.map ((fun (_, _, h) -> h) >> string)
        >> String.concat ""
    )
    |> String.concat "\n"
    |> printfn "%s"

let partOne =
    let trailheads =
        heightMap |> Array.collect (
            Array.choose (fun point ->
                let (_, _, height) = point
                match height = 0 with
                | true -> Some point
                | false -> None
            )
        )

    trailheads
    |> Array.map (
        getAllTrailsFromTrailhead
        >> Array.distinctBy Array.last
        >> Array.length
    )
    |> Array.sum

let partTwo =
    let trailheads =
        heightMap |> Array.collect (
            Array.choose (fun point ->
                let (_, _, height) = point
                match height = 0 with
                | true -> Some point
                | false -> None
            )
        )

    trailheads
    |> Array.map (
        getAllTrailsFromTrailhead
        >> Array.groupBy Array.last // Group by destination
        >> Array.map (snd >> Array.length) // Get number possibilities to go to each destination
        >> Array.sum // Sum all this numbers
    )
    |> Array.sum