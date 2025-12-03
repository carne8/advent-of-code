module AOC._2025.Day03

open System
open System.IO

let input =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (fun bank -> bank :> char seq)

let findPairOfBank bank =
    let b =
        bank
        |> Seq.indexed
        |> Seq.toArray

    let n = b |> Array.length

    let firstDigitIdx, firstDigit =
        b
        |> Array.take (n-1)
        |> Array.maxBy (snd >> Char.GetNumericValue >> int)
    let _, secondDigit =
        b
        |> Array.skip (firstDigitIdx + 1)
        |> Array.maxBy (snd >> Char.GetNumericValue >> int)

    sprintf "%c%c" firstDigit secondDigit |> int

let partOne () = input |> Array.sumBy findPairOfBank

let rec findBatteries n (bank: int64 list) =
    match n with
    | 0 -> []
    | _ ->
        let length = bank |> List.length
        let maxIdx, max =
            bank
            |> List.indexed
            |> List.take (length-n+1)
            |> List.maxBy snd

        printfn "%A" (bank, max)

        max :: findBatteries (n-1) bank[maxIdx+1..]

let partTwo () =
    input |> Array.sumBy (
        Seq.map (Char.GetNumericValue >> int64)
        >> Seq.toList
        >> findBatteries 12
        >> List.fold (fun acc e -> acc + string e) ""
        >> int64
    )