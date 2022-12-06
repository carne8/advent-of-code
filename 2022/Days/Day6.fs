module Day6

open System.Collections.Generic

let input = System.IO.File.ReadAllText "./Inputs/Day6.txt"

let readMarker = Stack<char>()

let getLastStackItem quantity stack =
    stack
    |> List.ofSeq
    |> List.chunkBySize quantity
    |> List.tryHead
    |> Option.defaultValue []

let findMarkerFromLength markerLength input =
    input
    |> Seq.indexed
    |> Seq.choose (fun (index, char) ->
        readMarker
        |> getLastStackItem markerLength
        |> List.distinct
        |> List.length
        <> markerLength
        |> function
            | true ->
                readMarker.Push char
                None
            | false -> Some index
    )
    |> Seq.head

printfn "Part 1 -> %A" (input |> findMarkerFromLength 4)
printfn "Part 2 -> %A" (input |> findMarkerFromLength 14)