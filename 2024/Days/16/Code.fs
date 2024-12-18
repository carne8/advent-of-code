module AdventOfCode._2024._16

open System.IO
open System.Collections.Generic

type Object =
    | Wall
    | EmptySpace
    | Start
    | End

let getInput () =
    let mutable startPoint = None
    let mutable endPoint = None

    let map =
        "input.txt"
        |> File.ReadAllLines
        |> Array.map (fun s -> s.ToCharArray())
        |> array2D
        |> Array2D.mapi (fun lineIdx columnIdx c ->
            match c with
            | '#' -> Wall
            | 'S' ->
                startPoint <- Some (lineIdx, columnIdx)
                Start
            | 'E' ->
                endPoint <- Some (lineIdx, columnIdx)
                End
            | '.'
            | _ -> EmptySpace
        )

    startPoint.Value,
    endPoint.Value,
    map

type Dir =
    Up | Right | Down | Left
    static member getDir start end' =
        [ Up, -1, 0 // Up
          Right, 0, 1 // Right
          Down, 1, 0 // Down
          Left, 0, -1 (*Left*) ]
        |> List.pick (fun (dir, deltaY, deltaX) ->
            let yCorrect = (end' |> fst) = (start |> fst) + deltaY
            let xCorrect = (end' |> snd) = (start |> snd) + deltaX

            match yCorrect, xCorrect with
            | true, true -> Some dir
            | _ -> None
        )


type AStarNode =
    { Point: int * int
      Parent: int * int
      Dir: Dir
      GCost: int
      HCost: int
      FCost: int }

let getDistance (endPoint: int * int) (startPoint: int * int) =
    let deltaX =
        (startPoint |> fst) - (endPoint |> fst)
        |> System.Math.Abs
    let deltaY =
        (startPoint |> snd) - (endPoint |> snd)
        |> System.Math.Abs

    deltaX + deltaY

let getPointNeighbours (map: _ array2d) (y, x) =
    [ -1, 0 // Up
      0, 1 // Right
      1, 0 // Down
      0, -1 (*Left*) ]
    |> List.map (fun (deltaY, deltaX) ->
        let posY = y + deltaY
        let posX = x + deltaX
        (posY, posX), map[posY, posX]
    )

// let dijkstra map startPoint endPoint =


let aStar map startPoint endPoint =
    let openList = new List<AStarNode>()
    let closedList = new List<AStarNode>()
    let mutable reachedEndPoint: AStarNode option = None

    let startPointHCost = getDistance endPoint startPoint
    { Point = startPoint
      Parent = startPoint
      /// Relative to parent
      Dir = Right
      GCost = 0
      HCost = startPointHCost
      FCost = startPointHCost }
    |> openList.Add

    while reachedEndPoint |> Option.isNone do
        let currentIdx, current =
            openList
            |> Seq.indexed
            |> Seq.minBy (snd >> _.FCost)

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
                    let dir = Dir.getDir current.Point pos
                    let gCost =
                        current.GCost
                        +
                        1
                        +
                        match current.Dir <> dir with
                        | true -> 1000
                        | false -> 0
                    let hCost = pos |> getDistance endPoint

                    Some {
                        Point = pos
                        Parent = current.Point
                        Dir = dir
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

let partOne () =
    let startPoint, endPoint, map = getInput()
    let path = aStar map startPoint endPoint

    path
    |> List.map _.Dir
    |> List.pairwise
    |> List.sumBy (fun (dirA, dirB) ->
        match dirA = dirB with
        | true -> 1
        | false -> 1001
    )

// 134596 to high