module AdventOfCode._2024._14

open System.IO
open System.Text.RegularExpressions

type Vector2(x, y) =
    member _.X: int = x
    member _.Y: int = y

    override this.ToString (): string = sprintf "<%i, %i>" this.X this.Y

type Robot =
    { InitialPosition: Vector2
      Velocity: Vector2 }

    static member private regex = Regex @"p=(\d+),(\d+) v=(-?\d+),(-?\d+)"
    static member parse = Robot.regex.Match >> fun m ->
        { InitialPosition =
            Vector2(
                m.Groups[1].Value |> int,
                m.Groups[2].Value |> int
            )
          Velocity =
            Vector2(
                m.Groups[3].Value |> int,
                m.Groups[4].Value |> int
            ) }

    static member private teleportIfNeeded (bathroom: Vector2) isX coord =
        let bathroomSize =
            match isX with
            | true -> bathroom.X
            | false -> bathroom.Y

        // printfn "Coord: %i; isX: %b; bathroomSize: %i; coord < 0: %b" coord isX bathroomSize (coord < 0)

        match coord < 0 with
        | true when coord % bathroomSize <> 0 -> bathroomSize + (coord % bathroomSize)
        | true -> 0
        | false -> coord % bathroomSize

    static member move
        deltaTime
        (bathroom: Vector2)
        { InitialPosition = pos; Velocity = vel }
        =
        Vector2(
            pos.X + deltaTime*vel.X |> Robot.teleportIfNeeded bathroom true,
            pos.Y + deltaTime*vel.Y |> Robot.teleportIfNeeded bathroom false
        )

module [<AutoOpen>] Bananas =
    let (|GreaterThan|_|) (x: int) (y: int) = y > x
    let (|LowerThan|_|) (x: int) (y: int) = y < x
    let (|Equals|_|) (x: int) (y: int) = x = y

let getRobots () =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map Robot.parse
    |> Array.toList

let visualize (bathroomSize: Vector2) (positions: Vector2 list) =
    Array.init
        bathroomSize.Y
        (fun lineIdx ->
            Array.init
                bathroomSize.X
                (fun columnIdx ->
                    positions
                    |> List.filter (fun v -> v.X = columnIdx && v.Y = lineIdx-1)
                    |> List.length
                    |> function
                        | 0 -> "."
                        | n -> n |> string
                )
            |> String.concat ""
        )
    |> String.concat "\n"


let partOne () =
    let bathroomSize = Vector2(101, 103) // Test value: Vector2(11, 7)
    let time = 100

    let sep =
        Vector2(
            (bathroomSize.X - 1) / 2,
            (bathroomSize.Y - 1) / 2
        )

    getRobots ()
    |> List.map (Robot.move time bathroomSize)
    |> List.groupBy (fun pos ->
        match pos.X, pos.Y with
        | Equals sep.X, _ -> 0
        | _, Equals sep.Y -> 0

        | LowerThan sep.X, LowerThan sep.Y -> 1
        | GreaterThan sep.X, LowerThan sep.Y -> 2
        | LowerThan sep.X, GreaterThan sep.Y -> 3
        | GreaterThan sep.X, GreaterThan sep.Y -> 4
        | _ ->
            printfn "%A could not be grouped" pos
            0
    )
    |> List.filter (fst >> (<>) 0)
    |> List.fold
        (fun acc group ->
            group
            |> snd
            |> List.length
            |> (*) acc
        )
        1

let partTwo () =
    let bathroomSize = Vector2(101, 103) // Test value: Vector2(11, 7)
    let mutable time = 0
    System.Console.Clear()

    while time < 7_800 do
        getRobots ()
        |> List.map (Robot.move time bathroomSize)
        |> visualize bathroomSize
        |> fun str ->
            System.Console.SetCursorPosition(0, 0)
            printfn "Time: %is" time
            System.Console.Write str

        time <- time+1
        System.Threading.Tasks.Task.Delay(2).Wait()

    // I watched it...
    // And found it :)

// Slow and stupid way to find it, but it works :D
let partTwoBis () =
    // Searching for a moment where 30 robots are aligned horizontally
    // I should use a array2D or a Dictionary instead of iterating on
    // all the points each times

    let bathroomSize = Vector2(101, 103)
    let mutable time = 0
    let mutable foundTree = None

    let initialCursorPos =
        System.Console.CursorLeft,
        System.Console.CursorTop

    while foundTree.IsNone && time < 10_000 do
        getRobots ()
        |> List.map (Robot.move time bathroomSize)
        |> fun l ->
            l
            |> List.tryFind (fun pos ->
                List.init 30 (fun i -> pos.X + i) // Expected values
                |> List.forall (fun x ->
                    l
                    |> List.tryFind (fun y -> y.Y = pos.Y && y.X = x)
                    |> Option.isSome
                )
            )
            |> Option.map (fun _ -> l)
        |> fun v -> foundTree <- v

        System.Console.SetCursorPosition initialCursorPos
        printfn "Checking at %i seconds" time
        time <- time+1

    match foundTree with
    | None -> printfn "Tree not found :("
    | Some foundTree ->
        System.Console.SetCursorPosition initialCursorPos
        printfn "Tree found at %i seconds !" (time - 1)

        foundTree
        |> visualize bathroomSize
        |> printfn "%s"
