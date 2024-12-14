module AdventOfCode._2024._03

open System.IO
open System.Text.RegularExpressions

let input = "input.txt" |> File.ReadAllText

let partOne =
    let regex = new Regex("mul\\((\\d+),(\\d+)\\)")
    let matches = input |> regex.Matches

    matches
    |> Seq.map (fun m ->
        let t1 = m.Groups |> Seq.item 1 |> _.Value |> int
        let t2 = m.Groups |> Seq.item 2 |> _.Value |> int

        t1*t2
    )
    |> Seq.sum


let partTwo =
    let regex = new Regex("(?:do\\(\\))|(?:don't\\(\\))|(?:mul\\((\\d+),(\\d+)\\))")
    let matches = input |> regex.Matches

    matches
    |> Seq.fold
        (fun (capturing, acc) m ->
            if m.Groups[0].Value = "do()" then
                (true, acc)
            elif m.Groups[0].Value = "don't()" then
                (false, acc)
            elif capturing then
                let t1 = m.Groups |> Seq.item 1 |> _.Value |> int
                let t2 = m.Groups |> Seq.item 2 |> _.Value |> int

                (true, acc + t1*t2)
            else
                (false, acc)
        )
        (true, 0)
    |> snd
