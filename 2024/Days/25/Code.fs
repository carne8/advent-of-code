module AdventOfCode._2024._25

open System.IO
open System.Collections.Generic

let getInput () =
    let input = "input.txt" |> File.ReadAllText

    let schemas =
        input.Split "\n\n"
        |> Array.map (fun rawSchema ->
            rawSchema.Split(
                "\n",
                System.StringSplitOptions.RemoveEmptyEntries
            )
            |> array2D
        )

    schemas
    |> List.ofArray
    |> List.groupBy (fun schema ->
        let mutable isLockSchema = true
        let mutable i = 0

        while isLockSchema && i < (schema |> Array2D.length2) do
            match schema[0, i] with
            | '.' -> isLockSchema <- false
            | '#' -> i <- i+1
            | _ -> failwith "Char not expected"

        isLockSchema
    )
    |> function
        | [ true, lockSchemas; false, keySchemas ] ->
            lockSchemas, keySchemas
        | _ -> failwith "Error occurred"

let getHeights (schema: char array2d) =
    let heights = new List<int>()

    let mutable x = 0
    while x < (schema |> Array2D.length2) do // For each column
        let mutable y = 0
        let mutable height = -1
        while y < (schema |> Array2D.length1) do // For each line
            match schema[y, x] with
            | '.' -> ()
            | '#' -> height <- height+1
            | _ -> ()

            y <- y+1

        heights.Add height
        x <- x+1

    heights |> Seq.toList


let partOne () =
    let locks, keys = getInput()

    let lockHeights = locks |> List.map getHeights
    let keyHeights = keys |> List.map getHeights

    let fittingPairs =
        [ for keyHeight in keyHeights do
            yield!
                lockHeights
                |> List.choose (fun lockHeight ->
                    List.exists2
                        (fun h1 h2 -> h1 + h2 > 5)
                        keyHeight
                        lockHeight
                    |> function
                        | false -> Some (keyHeight, lockHeight)
                        | true -> None
                ) ]

    fittingPairs |> List.length
