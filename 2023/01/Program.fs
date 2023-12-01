module Program

open System.IO

let input = "input.txt" |> File.ReadLines |> Seq.toArray

// ------ Part 1 ------

let getLineSimpleNumbers : string -> char array =
    _.ToCharArray()
    >> Array.filter System.Char.IsDigit

let part1 =
    input
    |> Array.map getLineSimpleNumbers
    |> Array.map (fun digits -> digits |> Array.head, digits |> Array.last)
    |> Array.map (fun (fst, snd) -> $"{fst}{snd}")
    |> Array.map int
    |> Array.sum

// ------ Part 2 ------

let digits =
    [| ("0", 0)
       ("1", 1)
       ("2", 2)
       ("3", 3)
       ("4", 4)
       ("5", 5)
       ("6", 6)
       ("7", 7)
       ("8", 8)
       ("9", 9)
       ("zero", 0)
       ("one", 1)
       ("two", 2)
       ("three", 3)
       ("four", 4)
       ("five", 5)
       ("six", 6)
       ("seven", 7)
       ("eight", 8)
       ("nine", 9) |]

let getLineNumbers (line: string) : int array =
    snd <| Array.fold
        (fun (line, foundNumbers) (char: char) ->
            let newLine: string = line + (string char)
            let foundNumber =
                digits
                |> Array.tryFind (fst >> newLine.EndsWith)
                |> Option.map snd

            newLine,
            [| yield! foundNumbers; if foundNumber |> Option.isSome then foundNumber.Value |]
        )
        ("", Array.empty)
        (line.ToCharArray())

let part2 =
    input
    |> Array.map getLineNumbers
    |> Array.map (fun digits -> digits |> Array.head, digits |> Array.last)
    |> Array.map (fun (fst, snd) -> $"{fst}{snd}")
    |> Array.map int
    |> Array.sum

// ------ Results ------
printfn "Part 1: %i" part1
printfn "Part 2: %i" part2