open System
open System.IO
open System.Text
open System.Collections.Generic

let input = File.ReadAllLines "./input.txt"
let testInput = File.ReadAllLines "./test-input.txt"

module Array =
    let duplicateWhen predicate array =
        array |> Array.collect (fun element ->
            match element |> predicate with
            | true -> [| element; element |]
            | false -> [| element |]
        )

type Point =
    Space | Galaxy

    static member parse = function
        | '.' -> Space
        | '#' -> Galaxy
        | _ -> failwith "Unknown char"

    static member raw = function Space -> '.' | Galaxy -> '#'

type IdGalaxy =
    | IdGalaxy of int * (int * int)
    static member id (IdGalaxy (id, (_, _))) = id
    static member x  (IdGalaxy (_,  (x, _))) = x
    static member y  (IdGalaxy (_,  (_, y))) = y

module GalaxyPair =
    let findShortestPath (g1, g2) =
        let g1x, g1y = g1 |> IdGalaxy.x, g1 |> IdGalaxy.y
        let g2x, g2y = g2 |> IdGalaxy.x, g2 |> IdGalaxy.y

        let deltaX = Math.Abs(g2x - g1x)
        let deltaY = Math.Abs(g2y - g1y)

        deltaX + deltaY


module Universe =
    let parse lines = lines |> Array.map (Seq.toArray >> Array.map Point.parse)

    let expand universe =
        let lineIsOnlySpace =
            Array.forall (function
                | Space -> true
                | Galaxy -> false)

        universe
        |> Array.duplicateWhen lineIsOnlySpace
        |> Array.transpose
        |> Array.duplicateWhen lineIsOnlySpace
        |> Array.transpose

    let show universe =
        universe
        |> Array.fold
            (fun (str: StringBuilder) line ->
                line
                |> Array.map Point.raw
                |> String
                |> str.AppendLine
            )
            (StringBuilder())
        |> fun strBuilder -> strBuilder.ToString()
        |> printfn "%s"

    let identifyGalaxies universe =
        let mutable i = 0
        let identifyGalaxy x y =
            let g = IdGalaxy (i, (x, y))
            i <- i + 1
            g

        universe
        |> Array.indexed
        |> Array.collect (fun (lineIdx, line) ->
            line
            |> Array.indexed
            |> Array.choose (fun (pointIdx, point) ->
                match point with
                | Space -> None
                | Galaxy -> identifyGalaxy lineIdx pointIdx |> Some
            )
        )

    let findGalaxyPairs (universe: IdGalaxy array) =
        universe |> Array.fold
            (fun pairs g1 ->
                universe
                |> Array.choose (function
                    | g2 when g1 = g2 -> None
                    | g2 -> Some (g1, g2))
                |> Array.append pairs
            )
            Array.empty
        |> Array.distinctBy (fun (g1, g2) ->
            [ g1 |> IdGalaxy.id
              g2 |> IdGalaxy.id ]
            |> List.sort
        )

let part1 input =
    input
    |> Universe.parse
    |> Universe.expand
    |> Universe.identifyGalaxies
    |> Universe.findGalaxyPairs
    |> Array.map GalaxyPair.findShortestPath
    |> Array.sum

// let part2 input = 0

printfn "Part 1: %A" (input |> part1)
// printfn "Part 2: %A" (testInput |> part2)