module Day2

// Points:   1       2        3
type RPS = Rock | Paper | Scissors

let lose = 0
let draw = 3
let win = 6

let getPointsFromRound (round: RPS * RPS) =
    match round with
        | Rock, Rock -> draw + 1
        | Rock, Paper -> win + 2
        | Rock, Scissors -> lose + 3

        | Paper, Rock -> lose + 1
        | Paper, Paper -> draw + 2
        | Paper, Scissors -> win + 3

        | Scissors, Rock -> win + 1
        | Scissors, Paper -> lose + 2
        | Scissors, Scissors -> draw + 3

let rawRounds =
    Inputs.day2.Split "\n" // This represent all rounds
    |> List.ofArray
    |> List.map (fun round -> [ round.[0]; round.[2] ]) // Split rounds in RPS

let puzzle1 =
    rawRounds
    |> List.map (List.map (function                     // Parse RPS
                            | 'A' | 'X' -> Rock
                            | 'B' | 'Y' -> Paper
                            | _ -> Scissors))
    |> List.map (fun round -> round.[0], round.[1])
    |> List.map getPointsFromRound
    |> List.sum

let puzzle2 =
    // 'X' -> I need to lose
    // 'Y' -> I need draw
    // 'Z' -> I need to win

    rawRounds
    |> List.map (fun round -> // Parsing
        let opponentRPS =
            match round.[0] with
            | 'A' -> Rock
            | 'B' -> Paper
            | _ -> Scissors

        opponentRPS, round.[1]
    )
    |> List.map (fun x ->
        match x with
        | x, 'Y' -> x

        | Rock, 'X' -> Scissors
        | Rock, 'Z' -> Paper

        | Paper, 'X' -> Rock
        | Paper, 'Z' -> Scissors

        | Scissors, 'X' -> Paper
        | Scissors, 'Z' -> Rock
        | _ -> failwith "Day 2: Puzzle 2 -> Bad char"
        |> fun y -> x |> fst, y
    )
    |> List.map getPointsFromRound
    |> List.sum


printfn "Puzzle 1 %A" puzzle1
printfn "Puzzle 2 %A" puzzle2