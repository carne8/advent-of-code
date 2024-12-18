module AdventOfCode._2024._18

open System.IO
open System.Collections.Generic

type Object =
    | Start
    | End
    | Byte
    | EmptySpace

let getEmptyMap () =
    let map = Array2D.create 71 71 EmptySpace

    let startPoint = 0, 0
    let endPoint = 70, 70
    map[fst startPoint, snd startPoint] <- Start
    map[fst endPoint, snd endPoint] <- End

    startPoint, endPoint, map

let getTestEmptyMap () =
    let map = Array2D.create 7 7 EmptySpace

    let startPoint = 0, 0
    let endPoint = 6, 6
    map[fst startPoint, snd startPoint] <- Start
    map[fst endPoint, snd endPoint] <- End

    startPoint, endPoint, map


let getBytes () =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (fun s ->
        match s.Split ',' with
        | [| rawX; rawY |] ->
            rawY |> string |> int,
            rawX |> string |> int
        | _ -> failwithf "Incorrect line: %A" s
    )

let fallBytes (map: Object array2d) count (bytes: (int * int) array) =
    bytes
    |> Array.take count
    |> Array.iter (fun (y, x) -> map[y, x] <- Byte)

module AStar =
    type Node =
        { Point: int * int
          Parent: int * int
          GCost: int
          HCost: int
          FCost: int }

    let getDistance (endPoint: int * int) (startPoint: int * int) =
        let deltaX = (startPoint |> fst) - (endPoint |> fst) |> System.Math.Abs
        let deltaY = (startPoint |> snd) - (endPoint |> snd) |> System.Math.Abs
        deltaX + deltaY

    let getPointNeighbours (map: _ array2d) (y, x) =
        [ -1, 0 // Up
          0, 1 // Right
          1, 0 // Down
          0, -1 (*Left*) ]
        |> List.choose (fun (deltaY, deltaX) ->
            let posY = y + deltaY
            let posX = x + deltaX
            try
                Some ((posY, posX), map[posY, posX])
            with _ -> None
        )

    let findPath map startPoint endPoint =
        let openList = new List<Node>()
        let closedList = new List<Node>()
        let mutable reachedEndPoint: Node option = None

        let startPointHCost = getDistance endPoint startPoint
        { Point = startPoint
          Parent = startPoint
          GCost = 0
          HCost = startPointHCost
          FCost = startPointHCost }
        |> openList.Add

        while reachedEndPoint |> Option.isNone do
            let currentIdx, current =
                openList
                |> Seq.indexed
                |> Seq.minBy (snd >> _.FCost)

            // printfn "\n---"
            // printfn "    current: %A" current
            // printfn "    openList: %A" (openList |> Seq.toList)
            // printfn "    closedList: %A" (closedList |> Seq.toList)

            openList.RemoveAt currentIdx |> ignore
            closedList.Add current

            match current.Point = endPoint with
            | true -> reachedEndPoint <- Some current
            | false ->
                current.Point
                |> getPointNeighbours map
                |> List.choose (fun (pos, object) ->
                    match object with
                    | End
                    | EmptySpace when closedList |> Seq.exists (_.Point >> (=) pos) |> not ->
                        let gCost = current.GCost + 1
                        let hCost = pos |> getDistance endPoint

                        Some {
                            Point = pos
                            Parent = current.Point
                            GCost = gCost
                            HCost = hCost
                            FCost = gCost + hCost
                        }
                    | _ -> None
                )
                |> List.iter (fun node ->
                    openList
                    |> Seq.tryFind (_.Point >> (=) node.Point) // Check if node has already been explored
                    |> function
                        | Some oldNode when node.FCost < oldNode.FCost ->
                            openList.Remove oldNode |> ignore
                            openList.Add node
                        | None -> openList.Add node
                        | _ -> ()
                )

        let rec getPathToStart l node =
            match node.Point = startPoint with
            | true -> node :: l
            | false -> getPathToStart (node :: l) (closedList.Find(_.Point >> (=) node.Parent))

        reachedEndPoint.Value |> getPathToStart []


let showMap (path: #(AStar.Node seq)) (map: Object array2d) =
    let stringMap =
        map |> Array2D.mapi (fun y x object ->
            match object with
            | Start -> "@"
            | End -> "E"
            | Byte -> "#"
            | EmptySpace ->
                path
                |> Seq.tryFind (_.Point >> (=) (y, x))
                |> Option.map (fun _ -> "O")
                |> Option.defaultValue "."
        )

    for y in 0..(stringMap |> Array2D.length1)-1 do
        let mutable line = ""
        for x in 0..(stringMap |> Array2D.length2)-1 do
            line <- line + stringMap[y, x]

        printfn "%s" line


let partOne () =
    let startPoint, endPoint, map = getEmptyMap()
    let bytes = getBytes()

    bytes |> fallBytes map 1024
    // showMap [] map

    let path = AStar.findPath map startPoint endPoint
    // showMap path map

    path |> List.length |> (+) -1

let partTwo () =
    let bytes = getBytes()

    let pathExist i =
        let startPoint, endPoint, map = getEmptyMap()
        bytes |> fallBytes map i

        try
            AStar.findPath map startPoint endPoint |> ignore
            true
        with _ -> false

    // Dichotomy
    let rec tryFind leftBound rightBound =
        match rightBound - leftBound = 1 with
        | true -> rightBound
        | false ->
            let middle = (rightBound - leftBound) / 2 + leftBound
            match pathExist middle with
            | true -> tryFind middle rightBound
            | false -> tryFind leftBound middle

    let blockingByte = tryFind 0 bytes.Length |> (+) -1
    bytes |> Array.item blockingByte |> fun (y, x) -> sprintf "%i,%i" x y