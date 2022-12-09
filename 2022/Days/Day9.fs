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
    "./Inputs/Day9.txt"
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

    let needToMove = Math.Abs distanceX > 1 || Math.Abs distanceY > 1

    let newTail =
        if needToMove && Math.Abs distanceX < Math.Abs distanceY then
            if distanceY < 0 then
                tailX + distanceX,
                tailY + distanceY + 1
            else
                tailX + distanceX,
                tailY + distanceY - 1
        elif needToMove && Math.Abs distanceX > Math.Abs distanceY then
            if distanceX < 0 then
                tailX + distanceX + 1,
                tailY + distanceY
            else
                tailX + distanceX - 1,
                tailY + distanceY
        elif needToMove then previousHead
        else tail

    head, newTail, previousHead

let moveRope direction = moveRopeHead direction >> moveRopeTail

let part1 =
    input
    |> Array.fold
        (fun (rope, tailPositions) direction ->
            let movedRope = rope |> moveRope direction
            let _, movedTail, _ = movedRope

            printfn "%A -> %A"
                direction
                (movedRope |> (fun (x, y, _) -> sprintf "Head %A, Tail %A" x y))
            movedRope, movedTail::tailPositions
        )
        (rope, [])
    |> snd
    |> List.rev
    |> List.distinct
    |> List.length

printfn "Day 9 -> Part 1: %A" part1