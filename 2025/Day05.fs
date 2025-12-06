module AOC._2025.Day05

open System
open System.IO

let ranges, products =
    let lines = "input.txt" |> File.ReadAllLines
    let emptyLineIndex = lines |> Array.findIndex String.IsNullOrEmpty

    lines
    |> Array.take emptyLineIndex
    |> Array.map (fun str ->
        match str.Split '-' with
        | [| low; high |] -> int64 low, int64 high
        | _ -> failwithf "Invalid range: %A" str
    ),
    lines
    |> Array.skip (emptyLineIndex + 1)
    |> Array.map int64

let isNumberInRange (a, b) n = a <= n && n <= b

let partOne () =
    products
    |> Array.filter (fun product ->
        ranges |> Array.exists (fun r -> isNumberInRange r product)
    )
    |> Array.length

type Range =
    { mutable Start: int64
      mutable End: int64 }

    static member create (a, b) = { Start = a; End = b }

    static member count r = r.End - r.Start + 1L

    override this.ToString() =
        sprintf "[%i, %i]" this.Start this.End

    member this.Intersects r =
        not (this.End < r.Start || r.End < this.Start)

    member this.Union r =
        this.Start <- min this.Start r.Start
        this.End <- max this.End r.End

let partTwo () =
    let ranges' =
        ranges
        |> Seq.sortBy fst
        |> Seq.map Range.create
        |> Seq.toArray

    let mutable combinedRanges = [ ranges'[0] ]

    for range in ranges'[1..] do
        combinedRanges
        |> List.tryFind _.Intersects(range)
        |> function
            | Some i ->
                printfn "%O - %O intersects" i range
                i.Union range
                printfn "Union: %O" i
            | None ->
                printfn "Add: %O" range
                combinedRanges <- range::combinedRanges

    combinedRanges
    |> List.sumBy Range.count