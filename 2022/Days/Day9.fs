module Day9

open System

type Direction =
    | Left
    | Right
    | Up
    | Down

    static member Parse str =
        match str with
        | "L" -> Left
        | "R" -> Right
        | "U" -> Up
        | _ -> Down

type Rope = (int * int) * (int * int) * (int * int)

let input =
    "./Inputs/Day9-test.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (String.split " ")
    |> Array.map (fun x -> Direction.Parse x.[0], int x.[1])
    |> Array.collect (fun (direction, distance) -> Array.create distance direction)

let rope = (0, 0), (0, 0), (0, 0)

let moveRopeHead direction rope : Rope =
    let head, tail, previousHead = rope
    let headX, headY = head

    match direction with
    | Up -> headX, headY + 1
    | Down -> headX, headY - 1
    | Left -> headX - 1, headY
    | Right -> headX + 1, headY
    , tail, head

let moveRopeTail (rope: Rope) : Rope =
    let head, tail, previousHead = rope
    let (headX, headY), (tailX, tailY) = head, tail
    let distanceX, distanceY = headX - tailX, headY - tailY

    if Math.Abs distanceX < Math.Abs distanceY then
        (tailX + distanceX,
         tailY + distanceY - 1)
        |> fun newTail -> head, newTail, previousHead
    elif Math.Abs distanceX > Math.Abs distanceY then
        (tailX + distanceX - 1,
         tailY + distanceY)
        |> fun newTail -> head, newTail, previousHead
    else head, previousHead, previousHead

let moveRope direction = moveRopeHead direction >> moveRopeTail

// ((4, 1), (3, 0), (4, 0))
// |> moveRopeTail

// ......    ......    ......
// ......    ......    ......
// ...... -> ...... -> ......
// ......    ....H.    ....H.
// ...TH.    ...T..    ...T..

let part1 =
    input
    |> Array.fold
        (fun (rope, tailPositions) direction ->
            let movedRope = rope |> moveRope direction
            let _, movedTail, _ = movedRope

            printfn "%A -> %A to %A"
                direction
                (rope |> (fun (x, y, _) -> sprintf "Head %A, Tail %A" x y))
                (movedRope |> (fun (x, y, _) -> sprintf "Head %A, Tail %A" x y))
            movedRope, movedTail::tailPositions
        )
        (rope, [])
    |> snd
    |> List.rev
    |> List.distinct
    |> List.length

printfn "Day 9 -> Part 1: %A" part1