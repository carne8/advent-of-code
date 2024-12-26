module AdventOfCode._2024._21

// Not an easy day...

open System
open System.IO
open System.Collections.Generic

module Array2D =
    let tryItem y x (arr: _ array2d) =
        try
            Some arr[y, x]
        with _ -> None

    let toSeq (arr: 'a array2d) : 'a seq =
        seq {
            for y in 0..(arr |> Array2D.length1)-1 do
                for x in 0..(arr |> Array2D.length2)-1 do
                    arr[y, x]
        }

    let tryFindIndex predicate arr =
        let mutable y = 0
        let mutable foundElement = None

        while foundElement.IsNone && y < (arr |> Array2D.length1) do
            let mutable x = 0
            while foundElement.IsNone && x < (arr |> Array2D.length2) do
                if arr[y, x] |> predicate then
                    foundElement <- Some (y, x)
                x <- x+1
            y <- y+1

        foundElement

let getDistance (y: int, x: int) (y', x') =
    (y - y' |> Math.Abs) + (x - x' |> Math.Abs)

type Key =
    | Number of int
    | Up
    | Left
    | Right
    | Down
    | Activate
    | Gap

    static member parse c =
        match c |> Char.IsDigit, c with
        | true, _ -> c |> string |> int |> Number
        | false, 'A' -> Activate
        | false, '^' -> Up
        | false, '>' -> Right
        | false, 'v' -> Down
        | false, '<' -> Left
        | _ -> failwithf "Incorrect char: %A" c

// let rec findPathsToKey keypad key targetKey =
//     let (y, x) = keypad |> Array2D.tryFindIndex ((=) key) |> Option.get
//     let (targetY, targetX) = keypad |> Array2D.tryFindIndex ((=) targetKey) |> Option.get

//     match (y, x) = targetPos with
//     | true -> []
//     | false ->
//         [ -1, 0, Up // Up
//           0, 1, Right // Right
//           1, 0, Down // Down
//           0, -1, Left // Left
//         ]
//         |> List.choose (fun (deltaY, deltaX, dir) ->
//             let y = y + deltaY
//             let x = x + deltaX

//             keypad
//             |> Array2D.tryItem y x
//             |> Option.bind (function
//                 | Gap -> None
//                 | _ -> Some ((y, x), dir)
//             )
//         )
//         |> List.minBy (fun (keyPos, _) -> getDistance keyPos targetPos)
//         |> function
//             | keyPos, dir when keyPos = targetPos -> [ dir ]
//             | keyPos, dir -> dir :: findPathToKey keypad keyPos targetPos

let rec findPathsToKey (keypad: _ array2d) key targetKey =
    let (y, x) = keypad |> Array2D.tryFindIndex ((=) key) |> Option.get
    let targetPos = keypad |> Array2D.tryFindIndex ((=) targetKey) |> Option.get

    let mutable paths: ((int * int) * Key) list list = [ [ (y, x), keypad[y, x] ] ]
    let mutable d = getDistance (y, x) targetPos

    let getNextPositions (y, x) =
        [ -1, 0, Up // Up
          0, 1, Right // Right
          1, 0, Down // Down
          0, -1, Left // Left
        ]
        |> List.choose (fun (deltaY, deltaX, dir) ->
            let y = y + deltaY
            let x = x + deltaX
            let currDistance = getDistance (y, x) targetPos

            keypad
            |> Array2D.tryItem y x
            |> Option.bind (function
                | Gap -> None
                | _ when currDistance < d -> Some ((y, x), dir)
                | _ -> None
            )
        )

    while d > 0 do
        paths <-
            paths |> List.collect (fun path ->
                let lastPathPos, _key = path |> Seq.last

                lastPathPos
                |> getNextPositions
                |> List.map (fun nextPos -> path @ [ nextPos ])
            )

        d <- d-1

    paths |> List.map (List.tail >> List.map snd)


let visualizePressedKeys (keys: #(Key seq)) =
    keys
    |> Seq.map (
        function
        | Up -> "^"
        | Right -> ">"
        | Down -> "v"
        | Left -> "<"
        | Activate -> "A"
        | _ -> ""
    )
    |> String.concat ""

let getInput () =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (Seq.map Key.parse)
    |> Array.map List.ofSeq
    |> List.ofArray

let numericKeypad =
    [ [ Number 7; Number 8; Number 9 ]
      [ Number 4; Number 5; Number 6 ]
      [ Number 1; Number 2; Number 3 ]
      [ Gap; Number 0; Activate ] ]
    |> array2D

let directionalKeypad =
    [ [ Gap; Up; Activate ]
      [ Left; Down; Right ] ]
    |> array2D

let numericMoves =
    List.allPairs
        [ for i in 0..9 do Number i
          Activate ]
        [ for i in 0..9 do Number i
          Activate ]
    |> List.map (fun (a, b) ->
        (a, b),
        findPathsToKey numericKeypad a b
        |> List.map (fun path -> path @ [ Activate ])
    )
    |> dict

let directionalMoves =
    List.allPairs
        [ Up; Activate; Left; Down; Right ]
        [ Up; Activate; Left; Down; Right ]
    |> List.map (fun (a, b) ->
        (a, b),
        findPathsToKey directionalKeypad a b
        |> List.map (fun path -> path @ [ Activate ])
    )
    |> dict


// Very sloow
let partOne () =
    let computeComplexity code keys =
        let numericPartOfCode =
            code
            |> List.choose (function
                | Number i -> i |> string |> Some
                | _ -> None
            )
            |> String.concat ""
            |> int

        numericPartOfCode * (keys |> List.length)

    let getSequenceForCode keypad initPos code =
        let pathsParts =
            code
            |> List.insertAt 0 initPos
            |> List.pairwise
            |> List.map (fun (a, b) ->
                findPathsToKey keypad a b
                |> List.map (fun path -> path @ [ Activate ])
            )

        pathsParts
        |> List.reduce (fun partPossibilities1 partPossibilities2 ->
            List.allPairs
                partPossibilities1
                partPossibilities2
            |> List.map (fun (a, b) -> a @ b)
        )

    getInput ()
    |> List.sumBy (fun code ->
        code
        |> getSequenceForCode numericKeypad Activate
        |> List.groupBy List.length
        |> List.minBy fst
        |> snd
        |> List.collect (
            getSequenceForCode directionalKeypad Activate
            >> List.groupBy List.length
            >> List.minBy fst
            >> snd
        )
        |> List.collect (getSequenceForCode directionalKeypad Activate)
        |> List.minBy List.length
        |> computeComplexity code
    )
// Also slow
let partOneBis () =
    let getKeystrokeForCode code =
        let numericKeys = code

        // For first robot
        let directionalKeysPossibilities =
            numericKeys
            |> List.insertAt 0 Activate
            |> List.pairwise
            |> List.fold
                (fun prevMoves (a, b) ->
                    match prevMoves with
                    | [] -> numericMoves[a, b]
                    | _ ->
                        List.allPairs
                            prevMoves
                            numericMoves[a, b]
                        |> List.map (fun (left, right) -> left @ right)
                )
                []

        // For second robot
        let directionalKeysPossibilities2 =
            directionalKeysPossibilities
            |> List.collect (fun directionalKeys ->
                directionalKeys
                |> List.insertAt 0 Activate
                |> List.pairwise
                |> List.fold // Cross product
                    (fun prevMoves (a, b) ->
                        match prevMoves with
                        | [] -> directionalMoves[a, b]
                        | _ ->
                            List.allPairs
                                prevMoves
                                directionalMoves[a, b]
                            |> List.map (fun (left, right) -> left @ right)
                    )
                    []
            )

        // For me
        let directionalKeysPossibilities3 =
            directionalKeysPossibilities2
            |> List.collect (fun directionalKeys ->
                directionalKeys
                |> List.insertAt 0 Activate
                |> List.pairwise
                |> List.fold // Cross product
                    (fun prevMoves (a, b) ->
                        match prevMoves with
                        | [] -> directionalMoves[a, b]
                        | _ ->
                            List.allPairs
                                prevMoves
                                directionalMoves[a, b]
                            |> List.map (fun (left, right) -> left @ right)
                    )
                    []
            )

        directionalKeysPossibilities3 |> List.minBy List.length


    let computeComplexity code keys =
        let numericPartOfCode =
            code
            |> List.choose (function
                | Number i -> i |> string |> Some
                | _ -> None
            )
            |> String.concat ""
            |> int

        printfn "%i * %i" (keys |> List.length) numericPartOfCode
        numericPartOfCode * (keys |> List.length)

    getInput()
    |> List.sumBy (fun code ->
        code
        |> getKeystrokeForCode
        |> computeComplexity code
    )


// --- Optimized solutions ---

let moveLengthDict = new Dictionary<_, _>()
let rec getMoveLength depth a b =
    match moveLengthDict.TryGetValue((depth, a, b)) with
    | true, v -> v
    | false, _ ->
        match depth <= 1 with
        | true -> directionalMoves[a, b] |> List.map List.length |> List.min |> int64
        | false ->
            directionalMoves[a, b]
            |> List.map (fun seq ->
                seq
                |> List.insertAt 0 Activate
                |> List.pairwise
                |> List.sumBy (fun (a, b) -> getMoveLength (depth-1) a b)
            )
            |> List.min
            |> fun v ->
                moveLengthDict.Add((depth, a, b), v)
                v

let getCodeLength intermediateRobotsCount code =
    code
    |> List.insertAt 0 Activate
    |> List.pairwise
    |> List.fold              // Get all possible sequences to do this code
        (fun sequences (a, b) -> // Cartesian product
            match sequences with
            | [] -> numericMoves[a, b]
            | _ ->
                List.allPairs
                    sequences
                    numericMoves[a, b]
                |> List.map (fun (left, right) -> left @ right)
        )
        []
    |> List.map (fun seq -> // Calculate the length of the required sequence for each code possible sequences
        seq
        |> List.insertAt 0 Activate
        |> List.pairwise
        |> List.map (fun (a, b) -> getMoveLength intermediateRobotsCount a b)
        |> List.sum
    )
    |> List.min // Get the shortest sequence

let computeComplexity code seqLength =
    let numericPartOfCode =
        code
        |> List.choose (function
            | Number i -> i |> string |> Some
            | _ -> None
        )
        |> String.concat ""
        |> int64

    numericPartOfCode * seqLength


let optimizedPartOne () =
    getInput()
    |> List.sumBy (fun code ->
        code
        |> getCodeLength 2
        |> computeComplexity code
    )

let partTwo () =
    getInput()
    |> List.sumBy (fun code ->
        code
        |> getCodeLength 25
        |> computeComplexity code
    )
