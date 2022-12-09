module Day9

// For this day, I apologize, I cheated.
// I used the solution of Gustav Gahm.
// -> https://github.com/gustavgahm/advent-of-code-2022

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

type Position = int * int
type Knot = Position
type Rope = Knot list

let input =
    "./Inputs/Day9.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (String.split " ")
    |> Array.map (fun x -> Direction.Parse x.[0], int x.[1])
    |> Array.collect (fun (direction, distance) -> Array.create distance direction)
    |> List.ofArray

let move direction knot =
    let x, y = knot

    match direction with
    | Left -> x - 1, y
    | Right -> x + 1, y
    | Up -> x, y + 1
    | Down -> x, y - 1

let follow knot2 knot1 : Position =
    let x1, y1 = knot1
    let x2, y2 = knot2

    let distanceX = x2 - x1
    let distanceY = y2 - y1

    if Math.abs distanceX <= 1 && Math.abs distanceY <= 1 then
        knot1
    else
        x1 + (Math.clamp distanceX (-1, 1)),
        y1 + (Math.clamp distanceY (-1, 1))

let simulate (direction: Direction) (rope: Rope) i : Rope =
    let position =
        if i = 0 then
            rope.[0] |> move direction
        else
            rope.[i] |> follow rope.[i - 1]

    rope |> List.replace i position

let updateKnots (history: Rope list) (direction: Direction) =
    let rope = history |> List.last
    let knots = [ 0 .. rope.Length - 1 ]
    let result = knots |> List.fold (simulate direction) rope

    history @ [ result ]

let updateRope count (moves: list<Direction>) =
    let knots: list<Position> = List.replicate count (0, 0)
    let history = [ knots ]

    moves |> List.fold updateKnots history

input
|> updateRope 2
|> List.map List.last
|> List.distinct
|> List.length
|> printfn "Day 9 -> Part 1: %A"

input
|> updateRope 10
|> List.map List.last
|> List.distinct
|> List.length
|> printfn "Day 9 -> Part 2: %A"