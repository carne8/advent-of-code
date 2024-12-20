module AdventOfCode._2024._20

#nowarn 40 // For the recursion
open System
open System.IO
open System.Collections.Generic

module Array2D =
    let tryItem y x (arr: _ array2d) =
        try
            Some arr[y, x]
        with _ -> None

    let toSeq (arr: 'a array2d) : 'a seq =
        seq {
            for y in 0..(arr |> Array2D.length1)-1 do
                for x in 0..(arr |> Array2D.length2)-1 do
                    arr[y, x]
        }

type Point =
    | RaceTrack of distanceFromEnd: int
    | Wall
    | Start
    | End

let getMap () =
    let mutable startPos = null |> unbox
    let mutable endPos = null |> unbox

    let map =
        "input.txt"
        |> File.ReadAllLines
        |> array2D
        |> Array2D.mapi (fun y x c ->
            match c with
            | '.' -> RaceTrack 0
            | '#' -> Wall
            | 'S' ->
                startPos <- y, x
                Start
            | 'E' ->
                endPos <- y, x
                End
            | _ -> failwith "Incorrect char"
        )

    startPos, endPos, map

/// Get the distance between two points (Manhattan distance)
let getDistance (aY: int, aX: int) (bY, bX) = Math.Abs(aY - bY) + Math.Abs(aX - bX)

/// Get point at left, right, up and down of a point
let getAroundPositions (y, x) =
    [ 0, 1 // Right
      0, -1 // Left
      -1, 0 // Up
      1, 0 (*Down*) ]
    |> List.map (fun (deltaY, deltaX) -> y + deltaY, x + deltaX)

/// Get all points that are at a certain distance from `pos`
let rec getAroundPointsAtDistance distance pos =
    match distance > 0 with
    | false -> [ pos ]
    | true ->
        pos
        |> getAroundPositions
        |> List.collect (getAroundPointsAtDistance (distance-1))
        |> List.filter ((<>) pos)
        |> List.distinct

/// Get all the points around a point in a range of `distance`
let getPointsAround (map: _ array2d) (distance: int) (posY, posX) =
    map
    |> Array2D.mapi (fun y x _ -> (y, x))
    |> Array2D.toSeq
    |> Seq.filter (fun (y, x) -> getDistance (posY, posX) (y, x) <= distance)
    |> Seq.toList


/// Set distance from the end for each point of the track
let computeRaceTrack start (map: Point array2d) =
    let list = new List<int * int>()
    let mutable prevPos = start
    let mutable pos = start
    list.Add pos |> ignore

    while map[pos |> fst, pos |> snd] <> End do
        let y, x = pos
        let nextPos =
                (y, x)
                |> getAroundPositions
                |> List.pick (fun (y, x) ->
                match (y, x) = prevPos with
                | true -> None
                | false ->
                    map
                    |> Array2D.tryItem y x
                    |> Option.bind (function
                        | End
                        | RaceTrack _ -> Some (y, x)
                        | _ -> None)
            )

        list.Add nextPos

        prevPos <- pos
        pos <- nextPos

    list
    |> Seq.indexed
    |> Seq.choose (fun (idx, (y, x)) ->
        match map[y, x] with
        | Start
        | End
        | RaceTrack _ ->
            let distanceFromEnd = list.Count - idx - 1
            map[y, x] <- RaceTrack distanceFromEnd
            Some (y, x, distanceFromEnd)
        | _ -> None
    )
    |> Seq.toList

let showMap (map: Point array2d) =
    for y in 0..(map |> Array2D.length1)-1 do
        let mutable line = ""
        for x in 0..(map |> Array2D.length2)-1 do
            map[y, x]
            |> function
                | Wall -> " # "
                | RaceTrack d ->
                    d
                    |> string
                    |> fun s ->
                        match s.Length with
                        | 1 -> " " + s + " "
                        | 2 -> s + " "
                        | _ -> failwith "Toto"
                | Start -> "S "
                | End -> "E "
            |> fun s -> line <- line + s

        printfn "%s" line

let partOne () =
    let start, _end', map = getMap()
    let raceTrack = computeRaceTrack start map

    let getPointCheats (y, x, distance) =
        (y, x)
        |> getAroundPointsAtDistance 2
        |> List.choose (fun (cheatedY, cheatedX) ->
            match map |> Array2D.tryItem cheatedY cheatedX with
            | Some End -> Some (distance - 2)
            | Some (RaceTrack cheatedDistance) when cheatedDistance < distance ->
                Some (distance - cheatedDistance - 2)
            | _ -> None
        )

    raceTrack
    |> List.collect getPointCheats
    |> List.filter (fun d -> d >= 100)
    |> List.length

let partTwo () =
    let start, _end', map = getMap()
    let raceTrack = computeRaceTrack start map

    let getPointCheats (y, x, distance) = // Really noy optimal
        (y, x)
        |> getPointsAround map 20
        |> List.choose (fun (cheatedY, cheatedX) ->
            match map |> Array2D.tryItem cheatedY cheatedX with
            | Some End ->
                Some (distance - getDistance (y, x) (cheatedY, cheatedX))
            | Some (RaceTrack cheatedDistance) when cheatedDistance < distance ->
                Some (distance - cheatedDistance - getDistance (y, x) (cheatedY, cheatedX))
            | _ -> None
        )

    raceTrack
    |> List.collect getPointCheats
    |> List.filter (fun d -> d >= 100)
    |> List.length