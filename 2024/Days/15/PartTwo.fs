module AdventOfCode._2024._15.PartTwo

open System.IO
open System.Collections.Generic

type Object =
    | Wall
    | Box of leftPart: bool
    | Robot

type Direction = Up | Right | Down | Left
module Direction =
    let apply dir (y, x) =
        match dir with
        | Up -> y-1, x
        | Right -> y, x+1
        | Down -> y+1, x
        | Left -> y, x-1

type Map = Dictionary<(int * int), Object>
module Map =
    let tryAt key (map: Map) =
        match map.TryGetValue key with
        | true, v -> Some v
        | false, _ -> None

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
        |> Array.collect (fun char ->
            match char with
            | '#' -> [| Some Wall; Some Wall |]
            | 'O' -> [| Some (Box true); Some (Box false) |]
            | '@' -> [| Some Robot; None |]
            | '.' -> [| None; None |]
            | _ -> [||]
        )
        |> Array.iteri (fun columnIdx object ->
            object |> Option.iter (fun object ->
                if object.IsRobot then
                    robotPos <- (lineIdx, columnIdx)
                map.Add((lineIdx, columnIdx), object)
            )
        )
    )

    robotPos, map

let getMoves rawMoves =
    rawMoves |> Array.choose (function
        | '^' -> Some Up
        | '>' -> Some Right
        | 'v' -> Some Down
        | '<' -> Some Left
        | '\n' -> None
        | _ -> failwith "Incorrect char"
    )

let moveItem (map: Map) pos dir item =
    let y, x = pos
    let newPos = (y, x) |> Direction.apply dir

    // match dir, item with
    // | Up, Box isLeft
    // | Down, Box isLeft ->
    //     let newY, newX = newPos
    //     printfn "    Moving a box up or down"

    //     // Move one part
    //     map.Remove((y, x)) |> ignore
    //     map.Add((newY, newX), Box isLeft) |> ignore
    //     printfn "        Moved %A to %A" (y, x) (newY, newX)

    //     // Move the other part
    //     let offset = if isLeft then +1 else -1
    //     map.Remove((y, x+offset)) |> ignore
    //     map.Add((newY, newX+offset), Box (not isLeft)) |> ignore
    //     printfn "        Moved %A to %A" (y, x+offset) (newY, newX+offset)
    // | _ ->
    printfn "    Trying %A to %A" pos newPos
    map.Remove(pos) |> ignore
    map.Add(newPos, item) |> ignore
    printfn "    Moved %A to %A" pos newPos

/// Returns true if succeed to move item
let rec canMove (map: Map) dir pos : (list<(int * int) * Direction * Object> * (int * int)) option =
    let currentItem = map[pos]
    let newPos = pos |> Direction.apply dir

    printfn "Trying to move %A" currentItem
    match currentItem, dir with
    | Wall, _ -> None // Can't move a wall
    | Box isLeft, Up
    | Box isLeft, Down ->
        let leftPartPos =
            match isLeft with
            | true -> pos
            | false -> pos |> Direction.apply Left
        let rightPartPos =
            match isLeft with
            | true -> pos |> Direction.apply Right
            | false -> pos

        let newLeftPartPos, newRightPartPos =
            leftPartPos |> Direction.apply dir,
            rightPartPos |> Direction.apply dir

        let leftBlocking = map |> Map.tryAt newLeftPartPos
        let rightBlocking =
            map
            |> Map.tryAt newRightPartPos
            // Prevent moving both parts of a box at the same time
            // This happens when 2 boxes are aligned
            |> Option.bind (function
                | Box false -> None
                | object -> Some object
            )

        let canMoveLeft =
            match leftBlocking with
            | None -> Some ([], (0, 0))
            | Some _ -> newLeftPartPos |> canMove map dir
        let canMoveRight =
            match rightBlocking with
            | None -> Some ([], (0, 0))
            | Some _ -> newRightPartPos |> canMove map dir

        match canMoveLeft, canMoveRight with
        | Some moveLeft, Some moveRight ->
            Some (
                [ yield! moveLeft |> fst
                  yield! moveRight |> fst
                  leftPartPos, dir, Box true
                  rightPartPos, dir, Box false ],
                newPos
            )
        | _ -> None

        // let newPosLeft, newPosRight =
        //     match isLeft with
        //     | true -> newPos, newPos |> Direction.apply Right
        //     | false -> newPos |> Direction.apply Left, newPos

        // let blockingItemLeft = map |> Map.tryAt newPosLeft
        // let blockingItemRight =
        //     map
        //     |> Map.tryAt newPosRight
        //     |> Option.bind (function // Prevent moving both parts of a box at the same time
        //         | Box false -> None
        //         | object -> Some object
        //     )

        // let canMoveLeft, canMoveRight =
        //     match blockingItemLeft, blockingItemRight with
        //     | None, None -> Some (fun _ -> newPosLeft |> Direction.apply dir), Some (fun _ -> newPosRight |> Direction.apply dir)
        //     | Some _, None -> canMove map dir newPosLeft, Some (fun _ -> newPosRight |> Direction.apply dir)
        //     | None, Some _ -> Some (fun _ -> newPosLeft |> Direction.apply dir), canMove map dir newPosRight
        //     | Some _, Some _ -> canMove map dir newPosLeft, canMove map dir newPosRight

        // match canMoveLeft, canMoveRight with
        // | Some moveLeft, Some moveRight ->
        //     Some (fun _ ->
        //         printfn "Applying move to box"
        //         printfn "  Moving left obstacle"
        //         moveLeft() |> ignore
        //         printfn "  Moving right obstacle"
        //         moveRight() |> ignore

        //         printfn "  Moving box from %A to %A" pos newPos
        //         currentItem |> moveItem map pos dir
        //         newPos
        //     )
        // | _ -> None
    | _ ->
        let blockingItem = newPos |> map.TryGetValue

        match blockingItem with
        | false, _ ->
            // No item forward, moving
            Some ([ pos, dir, currentItem ], newPos)
        | true, _ ->
            // item forward ! Try to move it
            match newPos |> canMove map dir with
            | Some moveItemForward ->
                Some (
                    [ yield! moveItemForward |> fst
                      pos, dir, currentItem ],
                    newPos
                )
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
            | true, Box isLeft -> if isLeft then '[' else ']'
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

    // map |> showMap

    // [ Down; Down
    //   Left; Left; Left; Left; Left
    //   Up;
    //   Right;
    //   Down;
    //   Right; Right
    //   Up ]
    // |> List.fold
    //     (fun robot move ->
    //         showMap map
    //         robot
    //         |> canMove map move
    //         |> Option.map (fun applyMove -> applyMove())
    //         |> Option.defaultValue robot
    //     )
    //     robotInitialPos
    // |> ignore
    // map |> showMap


    // let x =
    moves
    |> Array.fold
        (fun robot move ->
            robot
            |> canMove map move
            |> Option.map (fun moves ->
                moves
                |> fst
                |> List.distinct
                |> List.iter (fun (pos, dir, item) -> moveItem map pos dir item)

                moves |> snd
            )
            |> Option.defaultValue robot
        )
        robotInitialPos
    |> ignore

    // map |> showMap
    // x |> canMove map Up |> Option.iter(fst >> List.distinct >> List.iter (fun (pos, dir, item) -> moveItem map pos dir item))

    // let mutable robot = robotInitialPos
    // let mutable i = 0
    // let move () = moves[i]

    // robot <-
    //     robot
    //     |> canMove map (move())
    //     |> Option.map (fun applyMove -> applyMove())
    //     |> Option.defaultValue robot
    // map |> showMap
    // i<-i+1

    map |> Seq.sumBy (fun kv ->
        match kv.Value with
        | Box true ->
            let y, x = kv.Key
            100*y + x
        | _ -> 0
    )