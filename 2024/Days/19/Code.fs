module AdventOfCode._2024._19

#nowarn 40 // For the recursion
open System
open System.IO
open System.Collections.Generic

let getInput () =
    let txt = "input.txt" |> File.ReadAllLines

    let patterns =
        txt
        |> Array.head
        |> fun s -> s.Split ", "
        |> Array.toList

    let designs = txt[2..] |> Array.toList

    patterns, designs

let memoizeSnd (f: _ -> 'key -> 'res) : _ -> 'key -> 'res =
    let dict = new Dictionary<'key, 'res>()
    fun param1 param2 ->
        match dict.TryGetValue param2 with
        | true, v -> v
        | false, _ ->
            let v = f param1 param2
            dict.Add(param2, v)
            v

let rec getNeededPatterns = memoizeSnd <| fun patterns (designStr: string) ->
    let design = designStr |> Array.ofSeq

    match designStr with
    | "" -> Some []
    | _ ->
        [ 1..design.Length ]
        |> List.rev
        |> List.tryPick (fun splitIdx ->
            let subDesign, secondSubDesign =
                design
                |> Array.splitAt splitIdx
                |> fun (x, y) ->
                    x |> String,
                    y |> String

            match patterns |> List.tryFind ((=) subDesign) with
            | None -> None
            | Some pattern ->
                match secondSubDesign |> getNeededPatterns patterns with
                | None -> None
                | Some pattern2 -> Some (pattern :: pattern2)
        )

let rec countAllPossiblePatterns = memoizeSnd <| fun patterns (designStr: string) ->
    let design = designStr |> Array.ofSeq

    match designStr with
    | "" -> 0L
    | _ ->
        [ 1..design.Length ]
        |> List.sumBy (fun splitIdx ->
            let (subDesign: string), (secondSubDesign: string) =
                design
                |> Array.splitAt splitIdx
                |> fun (x, y) ->
                    x |> String,
                    y |> String

            match patterns |> List.exists ((=) subDesign) with
            | false -> 0L
            | true ->
                match patterns |> List.exists ((=) subDesign), secondSubDesign with
                | false, _ -> 0L
                | true, "" -> 1L
                | true, _ -> secondSubDesign |> countAllPossiblePatterns patterns
        )

let partOne () =
    let patterns, designs = getInput ()

    designs
    |> List.filter (getNeededPatterns patterns >> Option.isSome)
    |> List.length

let partTwo () =
    let patterns, designs = getInput ()
    designs |> List.sumBy (countAllPossiblePatterns patterns)

// Not performant way for part 2
// let rec getAllPossiblePatterns = memoizeSnd <| fun patterns (designStr: string) ->
//     let design = designStr |> Array.ofSeq

//     match designStr with
//     | "" -> []
//     | _ ->
//         [ 1..design.Length ]
//         |> List.rev
//         |> List.collect (fun splitIdx ->
//             let (subDesign: string), (secondSubDesign: string) =
//                 design
//                 |> Array.splitAt splitIdx
//                 |> fun (x, y) ->
//                     x |> String,
//                     y |> String

//             match patterns |> List.tryFind ((=) subDesign), secondSubDesign with
//             | None, _ -> []
//             | Some pattern, "" -> [ [ pattern ] ]
//             | Some pattern, _ ->
//                 match secondSubDesign |> getAllPossiblePatterns patterns with
//                 | [] -> []
//                 | patterns2 -> patterns2 |> List.map (fun p2 -> pattern::p2)
//         )
