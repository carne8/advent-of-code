module AOC._2025.Day08

open System.IO
open System.Collections.Generic

[<Struct>]
type Box =
    { X: int
      Y: int
      Z: int }

    static member distance box1 box2 =
        float (box1.X - box2.X) ** 2
        + float (box1.Y - box2.Y) ** 2
        + float (box1.Z - box2.Z) ** 2
        |> sqrt

let input =
    "input.txt"
    |> File.ReadLines
    |> Seq.map (fun line ->
        match line.Split ',' with
        | [| x; y; z |] -> { X = int x; Y = int y; Z = int z; }
        | _ -> failwith "Invalid box line"
    )
    |> Seq.toArray

let partOne () =
    let sortedByDistance =
        Seq.init input.Length (fun i ->
            Seq.init (input.Length-1) (fun j ->
                if j >= i then
                    struct (i, j+1)
                else
                    struct (i, j)
            )
        )
        |> Seq.concat
        |> Seq.map (fun struct (a, b) -> struct (min a b, max a b))
        |> Seq.distinct
        |> Seq.sortBy (fun struct (box1Idx, box2Idx) ->
            Box.distance input[box1Idx] input[box2Idx]
        )
        |> Seq.toArray

    let circuits: HashSet<int> array =
        input |> Array.mapi (fun boxIdx _ ->
            new HashSet<_> [| boxIdx |]
        )

    for struct (box1Idx, box2Idx) in sortedByDistance |> Array.take 1000 do
        let circuit1 = circuits[box1Idx]
        let circuit2 = circuits[box2Idx]

        printfn "Joining %A and %A" circuit1 circuit2

        circuit1.UnionWith circuit2
        circuit2 |> Seq.iter (fun boxIdx ->
            circuits[boxIdx] <- circuit1
        )

    circuits
    |> Array.distinct
    |> Array.map _.Count
    |> Array.sortDescending
    |> Array.take 3
    |> Array.fold (*) 1

let partTwo () =
    let sortedByDistance =
        Seq.init input.Length (fun i ->
            Seq.init (input.Length-1) (fun j ->
                if j >= i then
                    struct (i, j+1)
                else
                    struct (i, j)
            )
        )
        |> Seq.concat
        |> Seq.map (fun struct (a, b) -> struct (min a b, max a b))
        |> Seq.distinct
        |> Seq.sortBy (fun struct (box1Idx, box2Idx) ->
            Box.distance input[box1Idx] input[box2Idx]
        )
        |> Seq.toArray

    let circuits: HashSet<int> array =
        input |> Array.mapi (fun boxIdx _ ->
            new HashSet<_> [| boxIdx |]
        )

    let mutable i = 0
    let allCircuitsLinked () =
        circuits
        |> Seq.skip 1
        |> Seq.forall ((=) circuits[1])

    while not (allCircuitsLinked ()) && i < sortedByDistance.Length do
        let struct (box1Idx, box2Idx) = sortedByDistance[i]
        let circuit1 = circuits[box1Idx]
        let circuit2 = circuits[box2Idx]

        circuit1.UnionWith circuit2
        circuit2 |> Seq.iter (fun boxIdx ->
            circuits[boxIdx] <- circuit1
        )
        i <- i + 1

    let struct (box1Idx, box2Idx) = sortedByDistance[i-1]
    let box1 = input[box1Idx]
    let box2 = input[box2Idx]
    box1.X * box2.X