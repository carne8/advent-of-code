module AdventOfCode._2024._08

open System
open System.IO
open System.Numerics

// https://gist.github.com/krishnabhargav/da6686e295638d000aab
let rec gcd (a: int) (b: int) =
    let a', b' =
        a |> Math.Abs,
        b |> Math.Abs

    match (a',b') with
    | (x,y) when x = y -> x
    | (x,y) when x > y -> gcd (x-y) y
    | (x,y) -> gcd x (y-x)

let normalizeTkt (vec: Vector2) =
    let gcd =
        gcd (vec.X |> int) (vec.Y |> int)
        |> float32
    Vector2(vec.X / gcd, vec.Y / gcd)

type Antenna =
    | Antenna of position: Vector2 * frequency: char
    static member frequency (Antenna (_, freq)) = freq
    static member position (Antenna (pos, _)) = pos

let height, width, antennas =
    let lines = "input.txt" |> File.ReadAllLines

    let height = lines |> Array.length
    let width = lines |> Array.head |> String.length

    height,
    width,
    lines
    |> Array.indexed
    |> Array.collect (fun (lineIdx, line) ->
        line.ToCharArray()
        |> Array.indexed
        |> Array.choose (fun (charIdx, char) ->
            match char |> Char.IsLetterOrDigit with
            | true ->
                let pos = Vector2(lineIdx |> float32, charIdx |> float32)
                (pos, char) |> Antenna |> Some
            | false -> None
        )
    )
    |> Array.groupBy Antenna.frequency

let visualize preferShowingAntinodes antennas antinodes =
    Array.init height (fun lineIdx ->
        Array.init width (fun columnIdx ->
            let pos = Vector2(lineIdx |> float32, columnIdx |> float32)

            let isAntenna =
                antennas
                |> Array.tryPick (fun (freq, ants) ->
                    ants
                    |> Array.tryFind (Antenna.position >> (=) pos)
                    |> function
                        | None -> None
                        | Some _ -> Some freq
                )

            let isAntinode =
                antinodes
                |> Array.tryFind ((=) pos)
                |> Option.isSome

            match preferShowingAntinodes, isAntenna, isAntinode with
            | true, _        , true -> '#'
            | true, Some freq, _    -> freq
            | false, Some freq, _   -> freq
            | false, _       , true -> '#'
            | _ -> '.'
        )
    )
    |> Array.iter (fun line ->
        line |> Array.iter (printf "%c")
        printf "\n"
    )

let partOne =
    let antinodes =
        let isInBounds (vec: Vector2) =
            0f <= vec.Y && vec.Y < (width |> float32)
            && 0f <= vec.X && vec.X < (height |> float32)

        let isPlaceTookByAntenna freq pos =
            antennas
            |> Array.tryFind (fun (f, ants) ->
                match f = freq with
                | false -> false
                | true ->
                    ants
                    |> Array.tryFind (Antenna.position >> (=) pos)
                    |> Option.isSome
            )
            |> Option.isSome

        let isAntinodeValid freq antinode =
            antinode |> isInBounds
            && (antinode |> isPlaceTookByAntenna freq |> not)

        antennas |> Array.choose (fun (freq, ants) ->
            let antinodes =
                Array.allPairs ants ants
                |> Array.collect (fun (ant1, ant2) ->
                    let pos1 = ant1 |> Antenna.position
                    let pos2 = ant2 |> Antenna.position
                    let from1to2 = pos2 - pos1

                    let antinode1 = pos1 - from1to2
                    let antinode2 = pos2 + from1to2

                    [| if antinode1 |> isAntinodeValid freq then antinode1
                       if antinode2 |> isAntinodeValid freq then antinode2 |]
                )

            match antinodes with
            | [||] -> None
            | _ -> Some (freq, antinodes)
        )

    antinodes
    |> Array.collect snd
    |> Array.distinct
    |> Array.length

let partTwo =
    let antinodes =
        let isInBounds (vec: Vector2) =
            0f <= vec.Y && vec.Y < (width |> float32)
            && 0f <= vec.X && vec.X < (height |> float32)

        antennas |> Array.map (fun (freq, ants) ->
            freq,
            Array.allPairs ants ants |> Array.collect (fun (ant1, ant2) ->
                match ant1 <> ant2 with
                | false -> Array.empty
                | true ->
                    let pos1 = ant1 |> Antenna.position
                    let pos2 = ant2 |> Antenna.position
                    let from1To2 = pos2 - pos1 |> normalizeTkt

                    seq {
                        // First way
                        let mutable finished = false
                        let mutable i = 0

                        while not finished do
                            let vec = pos1 + (float32 i)*from1To2
                            i <- i+1
                            match vec |> isInBounds with
                            | true -> yield vec
                            | false -> finished <- true

                        // Second way
                        finished <- false
                        i <- 1
                        while not finished do
                            let vec = pos1 - (float32 i)*from1To2
                            i <- i+1
                            match vec |> isInBounds with
                            | true -> yield vec
                            | false -> finished <- true
                    }
                    |> Seq.toArray
            )
        )

    antinodes
    |> Array.collect snd
    |> Array.distinct
    |> Array.length