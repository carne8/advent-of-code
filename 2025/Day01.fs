module AOC._2025.Day01

open System.IO

/// Euclidean remainder, the proper modulo operation
let inline (%!) a b = (a % b + b) % b

let input =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (fun s ->
        match s[0] with
        | 'L' -> s.Substring 1 |> int |> (*) -1
        | 'R' -> s.Substring 1 |> int
        | _ -> failwithf "Invalid input: %A" s
    )

let partOne () =
    input |> Array.fold
        (fun (state, zeroCount) instruction ->
            let newState = (state + instruction) %! 100

            newState,
            if newState = 0 then zeroCount + 1 else zeroCount
        )
        (50, 0)

let partTwo () =
    input |> Array.fold
        (fun (state, zeroCount) instruction ->
            let mutable zeros = zeroCount
            let mutable newState = state

            let sign = sign instruction
            for _ = 1 to abs instruction do
                newState <- newState + sign
                if newState = 0 || newState = 100 then zeros <- zeros + 1
                newState <- newState %! 100

            // printfn "%2i - The dial is rotated %i to point at %i: %i zeros"
            //     state
            //     instruction
            //     newState
            //     (zeros - zeroCount)

            newState, zeros
        )
        (50, 0)