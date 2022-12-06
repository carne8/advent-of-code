module Day6

let findMarkerFromLength markerLength (input: string) =
    input
    |> Seq.indexed
    |> Seq.windowed markerLength
    |> Seq.tryFind (fun window ->
        window
        |> Seq.distinctBy snd
        |> Seq.length = markerLength
    )
    |> Option.map (Seq.last >> fst >> (+) 1)

"./Inputs/Day6.txt"
|> System.IO.File.ReadAllText
|> fun input ->
    printfn "Part 1 -> %A" (input |> findMarkerFromLength 4)
    printfn "Part 2 -> %A" (input |> findMarkerFromLength 14)