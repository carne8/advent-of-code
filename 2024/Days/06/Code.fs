module AdventOfCode._2024._06

open System.IO

let height, width, obstacles, guardPos =
    let mutable guardPos: int * int = null |> unbox
    let lines =
        "input.txt"
        |> File.ReadAllLines
    let obstacles =
        lines
        |> Array.indexed
        |> Array.collect (fun (lineIdx, line) ->
            line.ToCharArray()
            |> Array.indexed
            |> Array.choose (fun (columnIdx, char) ->
                match char with
                | '#' -> Some (lineIdx, columnIdx)
                | '^' ->
                    guardPos <- lineIdx, columnIdx
                    None
                | _ -> None
            )
        )

    lines.Length,
    lines |> Seq.head |> String.length,
    obstacles,
    guardPos

type Direction =
    | Top
    | Right
    | Bottom
    | Left

let getVisitedPlaces initialGuardPos obstacles =
    let mutable guardY, guardX = initialGuardPos
    let mutable direction = Top
    let mutable visitedPlaces = [ initialGuardPos ]
    let mutable rotationPoints = List.empty

    let isInBounds () =
        0 <= guardY && guardY < height
        && 0 <= guardX && guardX < width

    let looped () = // Does the guard have already turned here with the same direction ?
        rotationPoints
        |> List.filter ((=) (direction, guardY, guardX))
        |> List.length
        |> (<=) 2

    while isInBounds() && looped() |> not do
        let frontPos =
            match direction with
            | Top -> (guardY - 1, guardX)
            | Right -> (guardY, guardX + 1)
            | Bottom -> (guardY + 1, guardX)
            | Left -> (guardY, guardX - 1)

        let obstacle = obstacles |> Array.tryFind ((=) frontPos)
        let nextDir =
            match obstacle, direction with
            | None, _ -> None
            | Some _, Top -> Some Right
            | Some _, Right -> Some Bottom
            | Some _, Bottom -> Some Left
            | Some _, Left -> Some Top
        let nextY, nextX =
            match obstacle with
            | None ->
                visitedPlaces <- frontPos :: visitedPlaces
                frontPos
            | Some _ -> guardY, guardX

        match nextDir with
        | None -> ()
        | Some nextDir ->
            rotationPoints <- (nextDir, guardY, guardX) :: rotationPoints
            direction <- nextDir
        guardY <- nextY
        guardX <- nextX

    looped(),
    visitedPlaces
    |> List.distinct
    |> List.except [ visitedPlaces |> List.last ]

let partOne = getVisitedPlaces guardPos obstacles |> snd |> List.length

let partTwo =
    let path =
        obstacles
        |> getVisitedPlaces guardPos
        |> snd

    path
    |> List.filter (fun visitedPosition ->
        let newObstacles =
            Array.append
                obstacles
                [| visitedPosition |]

        newObstacles
        |> getVisitedPlaces guardPos
        |> fst
    )
    |> List.filter (fun (y, x) ->
        0 < y && y < height
        && 0 < x && x < width
    )
    |> List.length