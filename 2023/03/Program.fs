open System
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "./input.txt"
// let input = File.ReadAllLines "./2023/03/test-input.txt"

module Char =
    let (|IsDigit|_|) char =
        match char |> Char.IsDigit with
        | true -> char |> string |> int |> Some
        | false -> None

let parseLine (lineIdx: int) (line: string) : _ * _ =
    // ____.
    // _@@_.
    // ____.
    // .....

    let chars = line.ToCharArray()

    let foundNumbers = List<int * int>()
    let foundSymbol = List<int * char>()
    let mutable currentNumberOpt = None
    let mutable charIdx = 0

    for char in chars do
        match char, currentNumberOpt with
        | '.', None -> ()
        | '.', Some (idx, nb) ->
            currentNumberOpt <- None
            foundNumbers.Add (idx, int nb)

        | Char.IsDigit i, None ->            currentNumberOpt <- Some (charIdx, string i)
        | Char.IsDigit i, Some (idx, nb) ->  currentNumberOpt <- Some (idx, nb + string i)

        | symbol, None -> foundSymbol.Add (charIdx, symbol)
        | symbol, Some (idx, nb) ->
            currentNumberOpt <- None
            foundNumbers.Add (idx, int nb)
            foundSymbol.Add  (charIdx, symbol)

        charIdx <- charIdx + 1

    match currentNumberOpt with
    | None -> ()
    | Some (idx, nb) -> foundNumbers.Add (idx, int nb)

    (foundNumbers.ToArray() |> Array.map (fun (x, element) -> x, lineIdx, element)),
    (foundSymbol.ToArray() |> Array.map (fun (x, element) -> x, lineIdx, element))

let getAroundCoordinates x y length =
    [| x - 1, y
       x + length, y
       yield! [x-1..x+length] |> List.map (fun x -> x, y - 1)
       yield! [x-1..x+length] |> List.map (fun x -> x, y + 1) |]


let part1 =
    let foundNumbers, foundSymbols =
        input
        |> Array.mapi parseLine
        |> Array.unzip
        |> fun (a, b) -> a |> Array.concat, b |> Array.concat

    foundNumbers
    |> Array.choose (fun (x, y, nb) ->
        getAroundCoordinates x y (string nb |> String.length)
        |> Array.exists (fun (x, y) -> Array.exists (fun (sX, sY, _) -> sX = x && sY = y) foundSymbols)
        |> function
            | true -> Some nb
            | false -> None
    )
    |> Array.sum


// --- Part 2 ---
let getSymbolTouchingNumber (numbers: (int * int * int) array) symbolX symbolY =
    getAroundCoordinates symbolX symbolY 1
    |> Array.choose (fun (cooX, cooY) ->
        Array.tryFind
            (fun (nX, nY, n) ->
                let numberLength = n |> string |> String.length

                nX <= cooX
                && cooX < (nX + numberLength)
                && cooY = nY
            )
            numbers
    )
    |> Array.distinct

let part2 =
    let foundNumbers, foundSymbols =
        input
        |> Array.mapi parseLine
        |> Array.unzip
        |> fun (a, b) -> a |> Array.concat, b |> Array.concat

    foundSymbols
    |> Array.choose (function x, y, '*' -> Some (x, y) | _ -> None)
    |> Array.choose (fun (x, y) ->
        getSymbolTouchingNumber foundNumbers x y
        |> function
            | [| (_, _, nb1); (_, _, nb2) |] -> nb1 * nb2 |> Some
            | _ -> None
    )
    |> Array.sum

printfn "Part 1: %A" part1
printfn "Part 2: %A" part2