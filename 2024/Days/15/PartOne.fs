module AdventOfCode._2024._15.PartOne

open System.IO
open System.Collections.Generic

type Object =
    | Wall
    | Box
    | Robot

type Map = Dictionary<(int * int), Object>

// Parsing input
let getRawInput () =
    "input.txt"
    |> File.ReadAllText
    |> fun str -> str.Split "\n\n"
    |> function
        | [| a; b |] -> a, b.ToCharArray()
        | _ -> failwith "Failed to parse input"

let getMap (rawMap: string) =
    let map = new Map()
    let mutable robotPos: int * int = null |> unbox

    rawMap.Split "\n"
    |> Array.iteri (fun lineIdx line ->
        line.ToCharArray()
        |> Array.iteri (fun columnIdx char ->
            match char with
            | '#' -> map.Add((lineIdx, columnIdx), Wall)
            | 'O' -> map.Add((lineIdx, columnIdx), Box)
            | '@' ->
                robotPos <- lineIdx, columnIdx
                map.Add(robotPos, Robot)
            | _ -> ()
        )
    )

    robotPos, map

let getMoves rawMoves =
    rawMoves |> Array.choose (function
        | '^' -> Some (fun (y, x) -> y-1, x)
        | '>' -> Some (fun (y, x) -> y, x+1)
        | 'v' -> Some (fun (y, x) -> y+1, x)
        | '<' -> Some (fun (y, x) -> y, x-1)
        | '\n' -> None
        | _ -> failwith "Incorrect char"
    )

let moveItem (map: Map) pos newPos item =
    map.Remove(pos) |> ignore
    map.Add(newPos, item) |> ignore

/// Returns true if succeed to move item
let rec tryMove (map: Map) dir pos =
    let currentItem = map[pos]

    match currentItem with
    | Wall -> None // Can't move a wall
    | _ ->
        let newPos = pos |> dir
        let blockingItem = newPos |> map.TryGetValue

        match blockingItem with
        | false, _ ->
            // No item forward, moving
            currentItem |> moveItem map pos newPos
            Some newPos
        | true, _ ->
            // item forward ! Try to move it
            match newPos |> tryMove map dir with
            | Some _ ->
                currentItem |> moveItem map pos newPos
                Some newPos
            | None -> None

module [<AutoOpen>] Bananas =
    let (|GreaterThan|_|) (x: int) (y: int) = y > x
    let (|LowerThan|_|) (x: int) (y: int) = y < x
    let (|Equals|_|) (x: int) (y: int) = x = y

let showMap (map: Map) =
    let maxHeight, maxWidth =
        map.Keys |> Seq.fold
            (fun (maxY, maxX) (y, x) ->
                match y, x with
                | GreaterThan maxY, GreaterThan maxX -> (y, x)
                | GreaterThan maxY, _ -> (y, maxX)
                | _, GreaterThan maxX -> (maxY, x)
                | _ -> maxY, maxX
            )
            (0, 0)

    Array.init (maxHeight + 1) (fun lineIdx ->
        Array.init (maxWidth + 1) (fun columnIdx ->
            (lineIdx, columnIdx)
            |> map.TryGetValue
            |> function
            | false, _ -> '.'
            | true, Wall -> '#'
            | true, Box -> '0'
            | true, Robot -> '@'
        )
        |> System.String
    )
    |> String.concat "\n"
    |> printfn "%s"

let partOne () =
    let rawMap, rawMoves = getRawInput()
    let robotInitialPos, map = rawMap |> getMap
    let moves = rawMoves |> getMoves

    moves
    |> Array.fold
        (fun robot move ->
            robot
            |> tryMove map move
            |> Option.defaultValue robot
        )
        robotInitialPos
    |> ignore

    map |> Seq.sumBy (fun kv ->
        match kv.Value with
        | Box ->
            let y, x = kv.Key
            100*y + x
        | _ -> 0
    )