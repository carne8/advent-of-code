module AdventOfCode._2024._16

open System.IO
open System.Collections.Generic

type Object = Wall | EmptySpace | Start | End
type Map = Object array2d

let getMap () =
    let mutable startPoint = None

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
            | 'E' -> End
            | '.'
            | _ -> EmptySpace
        )

    startPoint.Value, map


// Firstly tried with A* but it didn't work
// I've probably missed something with my implementation
module FirstTry = // Not working
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


type Dir =
    Up | Right | Down | Left

    static member getNumericRep dir =
        match dir with
        | Up -> -1, 0
        | Right -> 0, 1
        | Down -> 1, 0
        | Left -> 0, -1

    static member getDirOfVec start end' =
        [ Up; Right; Down; Left ]
        |> List.pick (fun dir ->
            let deltaY, deltaX = dir |> Dir.getNumericRep
            let yCorrect = (end' |> fst) = (start |> fst) + deltaY
            let xCorrect = (end' |> snd) = (start |> snd) + deltaX

            match yCorrect, xCorrect with
            | true, true -> Some dir
            | _ -> None
        )

    static member rotateLeft dir =
        match dir with
        | Up -> Left
        | Right -> Up
        | Down -> Right
        | Left -> Down

    static member rotateRight dir =
        match dir with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    static member movePoint dir (y, x) =
        let deltaY, deltaX = dir |> Dir.getNumericRep
        y+deltaY, x+deltaX


type Node = { Cost: int; Point: int * int; Dir: Dir }

let findShortestPathCost (map: Map) start =
    let startNode = { Cost = 0; Point = start; Dir = Right }
    let queue = new PriorityQueue<Node, int>([ struct (startNode, startNode.Cost) ])
    let seenNodes = new HashSet<(int * int) * Dir>([ start, startNode.Dir ]) // I was stuck a long moment because of this line
    // I firstly used a List, but I learned a HashSet can be used !
    // A HashSet has a better complexity to check if an element is present

    let mutable foundEnd: Node option = None

    while queue.Count > 0 && foundEnd.IsNone do
        let node = queue.Dequeue()
        seenNodes.Add (node.Point, node.Dir) |> ignore

        [ node.Point |> Dir.movePoint node.Dir, node.Dir, node.Cost + 1
          node.Point, node.Dir |> Dir.rotateLeft, node.Cost + 1000
          node.Point, node.Dir |> Dir.rotateRight, node.Cost + 1000 ]
        |> List.iter (fun ((y, x), dir, cost) ->
            let object =
                try
                    Some map[y, x]
                with _ -> None

            match object with
            | None -> ()
            | Some Wall -> ()
            | Some _ when seenNodes.Contains ((y, x), dir) -> ()
            | Some End ->
                foundEnd <- Some { Cost = cost; Point = y, x; Dir = dir }
            | Some _ ->
                queue.Enqueue(
                    { Cost = cost; Point = y, x; Dir = dir },
                    cost
                )
        )

    foundEnd |> Option.map _.Cost


type NodeWithParent = { Cost: int; Point: int * int; Dir: Dir; Parent: NodeWithParent option } // Absolutely not optimized

let findShortestPaths (map: Map) start =
    let startNode = { Cost = 0; Point = start; Dir = Right; Parent = None }
    let queue = new PriorityQueue<NodeWithParent, int>([ struct (startNode, startNode.Cost) ])
    let lowestCostToNode = new Dictionary<(int * int) * Dir, int>(dict [ (start, startNode.Dir), 0 ])

    let paths = new List<NodeWithParent list>()
    let mutable bestPathCost = None
    let mutable finished = false

    while queue.Count > 0 && not finished do
        let node = queue.Dequeue()

        match lowestCostToNode.TryGetValue((node.Point, node.Dir)) with
        | true, lowestCost when node.Cost > lowestCost -> ()
        | _ ->
            lowestCostToNode[(node.Point, node.Dir)] <- node.Cost

            [ node.Point |> Dir.movePoint node.Dir, node.Dir, node.Cost + 1
              node.Point, node.Dir |> Dir.rotateLeft, node.Cost + 1000
              node.Point, node.Dir |> Dir.rotateRight, node.Cost + 1000 ]
            |> List.iter (fun ((y, x), dir, cost) ->
                let object =
                    try
                        Some map[y, x]
                    with _ -> None

                match object, lowestCostToNode.TryGetValue(((y, x), dir)) with
                | None, _ -> ()
                | Some Wall, _ -> ()
                | Some End, _ -> // We manage the lowest cost to then End nodes with `bestPathCost`
                    match bestPathCost with
                    | Some bestPathCost when cost > bestPathCost ->
                        // We found all paths with the minimum cost
                        finished <- true
                    | _ ->
                        bestPathCost <- Some cost
                        let lastNode = { Cost = cost; Point = y, x; Dir = dir; Parent = Some node }

                        let path =
                            [ let mutable node = lastNode
                              node

                              while node.Parent.IsSome do
                                let parent = node.Parent.Value
                                node <- parent
                                parent ]

                        paths.Add path
                | Some _, (true, lowestCost) when cost > lowestCost -> ()
                | Some _, _ ->
                    queue.Enqueue(
                        { Cost = cost; Point = y, x; Dir = dir; Parent = Some node },
                        cost
                    )
            )

    paths


let partOne () =
    let startPoint, map = getMap()

    findShortestPathCost map startPoint
    |> Option.get

let partTwo () =
    let startPoint, map = getMap()

    let paths = findShortestPaths map startPoint

    paths
    |> Seq.collect (List.map _.Point)
    |> Seq.distinct
    |> Seq.length