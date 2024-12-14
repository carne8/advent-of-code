module AdventOfCode._2024._07

open System
open System.IO

let parseNumbers (numbers: string) =
    numbers.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64

let tests =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (fun line ->
        line.Split ':'
        |> function
            | [| left; right |] ->
                left |> int64, right |> parseNumbers
            | _ -> failwith "Failed to parse input"
    )


// https://www.fssnip.net/2A/title/Cartesian-product-of-n-lists
let rec cartesian lstlst =
    match lstlst with
    | h::[] ->
        List.fold (fun acc elem -> [elem]::acc) [] h
    | h::t ->
        List.fold (fun cacc celem ->
            (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
            ) [] (cartesian t)
    | _ -> []

type Operator =
    | Add
    | Multiply
    | Concatenation

    static member getFunc operator =
        match operator with
        | Add -> (+)
        | Multiply -> (*)
        | Concatenation ->
            fun i1 i2 -> sprintf "%i%i" i1 i2 |> int64

let applyPipeline pipeline numbers =
    let mutable acc = numbers |> Seq.head

    for i in 0..(numbers |> Seq.length |> (+) -2) do
        let func =
            pipeline
            |> Seq.item i
            |> Operator.getFunc

        acc <- func acc (numbers |> Seq.item (i+1))

    acc

let partOne =
    let ops = [ Add; Multiply ]

    tests
    |> Array.choose (fun (expectedValue, numbers) ->
        let pipelineLength = numbers |> Array.length |> (+) -1 // Number of operations
        let pipelines =
            List.init pipelineLength (fun _ -> ops)
            |> cartesian

        pipelines
        |> List.filter (fun pipeline ->
            numbers
            |> applyPipeline pipeline
            |> (=) expectedValue
        )
        |> function
            | [] -> None
            | _ -> Some expectedValue
    )
    |> Array.sum


let partTwo = // Slow... May need some optimisations
    let ops = [ Add; Multiply; Concatenation ]

    tests
    |> Array.choose (fun (expectedValue, numbers) ->
        let pipelineLength = numbers |> Array.length |> (+) -1 // Number of operations
        let pipelines =
            List.init pipelineLength (fun _ -> ops)
            |> cartesian

        pipelines
        |> List.filter (fun pipeline ->
            numbers
            |> applyPipeline pipeline
            |> (=) expectedValue
        )
        |> function
            | [] -> None
            | _ -> Some expectedValue
    )
    |> Array.sum
