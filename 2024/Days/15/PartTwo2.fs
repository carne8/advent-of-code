module AdventOfCode._2024._15.PartTwo

open System.IO
open System.Collections.Generic

type Object =
    | Wall
    | Box
    | Robot

type Map = Dictionary<(int * float), Object>

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
    let mutable robotPos: int * float = null |> unbox

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
        | '>' -> Some (fun (y, x) -> y, x+0.5)
        | 'v' -> Some (fun (y, x) -> y+1, x)
        | '<' -> Some (fun (y, x) -> y, x-0.5)
        | '\n' -> None
        | _ -> failwith "Incorrect char"
    )

let moveItem (map: Map) pos newPos item =
    map.Remove(pos) |> ignore
    map.Add(newPos, item) |> ignore

/// Returns true if succeed to move item
let rec tryMove (map: Map) dir pos =
    printfn "%A" pos
    let currentItem = map[pos]

    match currentItem with
    | Wall -> None // Can't move a wall
    | _ ->
        let newPos = pos |> dir
        let blockingItem =
            newPos
            |> map.TryGetValue
            |> function
                | true, v -> true, v
                | false, _ ->
                    map.TryGetValue((
                        newPos |> fst,
                        newPos |> snd |> System.Math.Truncate
                    ))

        match blockingItem with
        | false, _ ->
            // No item forward, moving
            currentItem |> moveItem map pos newPos
            Some newPos
        | true, b ->
            printfn "Blocked: %A" b
            // item forward ! Try to move it
            match newPos |> tryMove map dir with
            | Some _ ->
                currentItem |> moveItem map pos newPos
                Some newPos
            | None -> None

module [<AutoOpen>] Bananas =
    let (|GreaterThan|_|) (x: System.IComparable) (y: System.IComparable) = y > x
    let (|LowerThan|_|) (x: System.IComparable) (y: System.IComparable) = y < x
    let (|Equals|_|) (x: System.IComparable) (y: System.IComparable) = x = y

let showMap (map: Map) =
    let maxHeight, maxWidth =
        map.Keys
        |> Seq.fold
            (fun (maxY, maxX) (y, x) ->
                match y, x with
                | GreaterThan maxY, GreaterThan maxX -> (y, x)
                | GreaterThan maxY, _ -> (y, maxX)
                | _, GreaterThan maxX -> (maxY, x)
                | _ -> maxY, maxX
            )
            (0, 0)
        |> fun (y, x) -> y, x |> System.Math.Ceiling |> int

    Array.init (maxHeight + 1) (fun lineIdx ->
        Array.init (maxWidth + 1) (fun columnIdx ->
            (lineIdx, float columnIdx)
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

let partTwo () =
    let rawMap, rawMoves = getRawInput()
    let robotInitialPos, map = rawMap |> getMap
    let moves = rawMoves |> getMoves

    (3, 5.) |> tryMove map (fun (y, x) -> y+2, x)
    map |> showMap

    (5, 5.) |> tryMove map (fun (y, x) -> y, x-1.5)
    map |> showMap
    (5, 3.0) |> tryMove map (fun (y, x) -> y, x+0.5)
    map |> showMap

    // (5, 7)
    // |> canMove map (fun (y, x) -> y-1, x)
    // |> Option.map (fun applyMove -> applyMove())
    // map |> showMap

    moves
    |> Array.fold
        (fun robot move ->
            robot
            |> tryMove map move
            |> Option.defaultValue robot
        )
        robotInitialPos
    |> ignore

    // map |> Seq.sumBy (fun kv ->
    //     match kv.Value with
    //     | Box ->
    //         let y, x = kv.Key
    //         100*y + x
    //     | _ -> 0
    // )