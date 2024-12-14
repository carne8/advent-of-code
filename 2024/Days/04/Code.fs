module AdventOfCode._2024._04

open System.IO

let input =
    "input.txt"
    |> File.ReadAllLines
    |> Array.ofSeq
    |> Array.mapi (fun lineIdx line ->
        line
        |> Array.ofSeq
        |> Array.mapi (fun columnIdx char -> lineIdx, columnIdx, char)
    )
let xmas = "XMAS".ToCharArray()

let getNextChar char =
    xmas
    |> Array.findIndex ((=) char)
    |> (+) 1
    |> Array.get xmas

let getDirections (lineIdx, columnIdx, char) =
    let expectedChar = char |> getNextChar

    [| (0, -1) // Left
       (0, +1) // Right
       (-1, 0) // Top
       (+1, 0) // Bottom
       (-1, -1) // Top left
       (-1, +1) // Top right
       (+1, -1) // Bottom left
       (+1, +1) // Bottom right
       |]
    |> Array.choose (fun (deltaY, deltaX) ->
        input
        |> Array.tryItem (lineIdx + deltaY)
        |> Option.bind (Array.tryItem (columnIdx + deltaX))
        |> Option.bind (fun (y, x, c) ->
            match c = expectedChar with
            | true -> Some ((deltaY, deltaX), (y, x, c))
            | false -> None
        )
    )

let countXmasFromStart start =
    start
    |> getDirections
    |> Array.filter (fun (delta, (y, x, char)) ->
        let mutable finished: bool option = None
        let mutable expectedChar = char |> getNextChar
        let mutable y = y
        let mutable x = x

        while finished.IsNone do
            y <- y + (delta |> fst)
            x <- x + (delta |> snd)
            let char =
                input
                |> Array.tryItem y
                |> Option.bind (Array.tryItem x)
                |> Option.map (fun (_, _, c) -> c)

            match char with
            | Some char when char = expectedChar && char = 'S' -> finished <- Some true
            | Some char when char = expectedChar -> expectedChar <- char |> getNextChar
            | _ -> finished <- Some false

        finished.Value
    )
    |> Array.length

let isXMasValid start =
    let (y, x, _) = start

    [ [y-1, x-1 ; y+1, x+1]
      [y+1, x-1 ; y-1, x+1] ]
    |> List.map (
        List.choose (fun (y, x) ->
            input
            |> Array.tryItem y
            |> Option.bind (Array.tryItem x)
            |> Option.map (fun (_, _, c) -> c)
        )
    )
    |> List.forall (fun diag ->
        diag |> List.contains 'S'
        && diag |> List.contains 'M'
    )

let partOne =
    input
    |> Array.collect (Array.filter (fun (_, _, c) -> c = 'X')) // XMAS starts
    |> Array.sumBy countXmasFromStart

let partTwo =
    input
    |> Array.collect (fun line ->
        line
        |> Array.filter (fun (y, x, c) ->
            c = 'A' && isXMasValid (y, x, c)
        )
    )
    |> Array.length
