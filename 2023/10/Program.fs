open System
open System.IO

let input = File.ReadAllLines "./input.txt" |> List.ofArray
let testInput = File.ReadAllLines "./test-input.txt" |> List.ofArray

type Pipe =
    | Start
    | NorthSouth // |
    | EastAndWest // -
    | NorthAndEast // L
    | NorthAndWest // J
    | SouthAndWest // 7
    | SouthAndEast // F

    static member parse (char: Char) =
        match char with
        | 'S' -> Some Start
        | '|' -> Some NorthSouth
        | '-' -> Some EastAndWest
        | 'L' -> Some NorthAndEast
        | 'J' -> Some NorthAndWest
        | '7' -> Some SouthAndWest
        | 'F' -> Some SouthAndEast
        | _ -> None

    static member getDirections pipe =
        match pipe with
        | Start -> [ -1, 0; 0, 1; 1, 0; 0, -1 ]
        | NorthSouth -> [ -1, 0; 1, 0 ]
        | EastAndWest -> [ 0, -1; 0, -1 ]
        | NorthAndEast -> [ -1, 0; 0, 1 ]
        | NorthAndWest -> [ -1, 0; 0, -1 ]
        | SouthAndWest -> [ 1, 0; 0, -1 ]
        | SouthAndEast -> [ 1, 0; 0, 1 ]

    static member getNext (prevX, prevY) pipe =

        match pipe, prevX, prevY with
        | NorthSouth, -1, 0 -> 1, 0
        | NorthSouth, 1, 0 -> -1, 0

        | EastAndWest, 0, -1 -> 0, 1
        | EastAndWest, 0, 1 -> 0, -1

        | NorthAndEast, -1, 0 -> 0, 1
        | NorthAndEast, 0, 1 -> -1, 0

        | NorthAndWest, -1, 0 -> 0, -1
        | NorthAndWest, 0, -1 -> -1, 0

        | SouthAndWest, 1, 0 -> 0, -1
        | SouthAndWest, 0, -1 -> 1, 0

        | SouthAndEast, 1, 0 -> 0, 1
        | SouthAndEast, 0, 1 -> 1, 0

        | x, y, z -> failwithf "Impossible situation: %A, %i %i" x y z


let parseLine str =
    str
    |> Seq.map Pipe.parse
    |> Seq.toList


let part1 input =
    let pipes = input |> List.map parseLine
    let pipesMatrix = pipes |> array2D

    let startX, startY, _ =
        pipes
        |> List.mapi (fun lineIndex line ->
            line
            |> List.mapi (fun pipeIndex pipe -> lineIndex, pipeIndex, pipe)
        )
        |> List.concat
        |> List.find (
            function
            | (_, _, Some Start) -> true
            | _ -> false)

    let mutable finished = false
    let mutable i = 0
    let mutable prevLineIdx, prevColumnIdx = startX, startY
    let mutable lineIdx, columnIdx = startX, startY

    while not finished do
        let currentCase = pipesMatrix[lineIdx, columnIdx]

        let nextLine, nextColumn =
            match currentCase with
            | None -> failwith "Not on a pipe !"
            | Some Start when i > 0 ->
                finished <- true
                0, 0

            | Some Start when i = 0 ->
                [ -1, 0
                  0, 1
                  1, 0
                  0, -1 ]
                |> List.map (fun (x, y) -> x, y, pipesMatrix[Math.Max(0, lineIdx+x), Math.Max(0, columnIdx+y)])
                |> List.map (fun x -> printfn "%A" x; x)
                |> List.pick (fun (x, y, pipe) ->
                    match pipe, x, y with
                    | Some NorthSouth, -1, 0 | Some NorthSouth, +1, 0 // |
                    | Some EastAndWest, 0, -1 | Some EastAndWest, 0, +1 // -
                    | Some NorthAndEast, +1, 0 | Some NorthAndEast, 0, -1 // L
                    | Some NorthAndWest, +1, 0 | Some NorthAndWest, 0, +1 // J
                    | Some SouthAndWest, -1, 0 | Some SouthAndWest, 0, +1 // 7
                    | Some SouthAndEast, -1, 0 | Some SouthAndEast, 0, -1 -> Some (x, y) // F
                    | _ -> None
                )

            | Some pipe ->
                let delta = prevLineIdx - lineIdx, prevColumnIdx - columnIdx
                pipe |> Pipe.getNext delta

        prevLineIdx <- lineIdx
        prevColumnIdx <- columnIdx
        lineIdx <- lineIdx + nextLine
        columnIdx <- columnIdx + nextColumn

        i <- i + 1

    float i / 2.
    |> Math.Floor
    |> int

printfn "Part 1: %i" (input |> part1)