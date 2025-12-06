module AOC._2025.Day06

open System
open System.IO

let inputPartOne : (int64 array * (int64 -> int64 -> int64)) array =
    "input.txt"
    |> File.ReadLines
    |> Seq.map (fun line ->
        let line = line.Split(
            ' ',
            StringSplitOptions.TrimEntries
            ||| StringSplitOptions.RemoveEmptyEntries
        )
        line
    )
    |> Seq.toArray
    |> fun arr ->
        Array.init (Array.length arr[0]) (fun columnIdx ->
            Array.init
                (Array.length arr - 1)
                (fun lineIdx -> int64 <| arr[lineIdx][columnIdx]),

            match arr[Array.length arr - 1][columnIdx] with
            | "+" -> (+)
            | _ -> (*)
        )

let partOne () =
    inputPartOne |> Array.sumBy (fun (numbers, op) ->
        numbers[1..]
        |> Array.fold
            op
            numbers[0]
    )

let transpose (arr: _ array array) =
    Array.init
        (Array.length arr[0])
        (fun i -> Array.init (Array.length arr) (fun j -> arr[j][i]))

module Seq =
    let transpose (s: #seq<#seq<_>>) =
        Seq.init
            (s |> Seq.head |> Seq.length)
            (fun i ->
                Seq.init
                    (Seq.length s)
                    (fun j ->
                        s
                        |> Seq.item j
                        |> Seq.item i
                    )
            )

type Problem =
    { Numbers: int64 seq
      Operation: int64 -> int64 -> int64 }

    static member columnToNumber (chars: char seq) =
        chars
        |> Seq.filter Char.IsDigit
        |> Seq.toArray
        |> String
        |> int64

    static member fromStrings (strings: #seq<seq<char>>) =
        { Numbers =
            strings |> Seq.map Problem.columnToNumber
          Operation =
            strings
            |> Seq.last
            |> Seq.pick (function
                | '+' -> Some (+)
                | '*' -> Some (*)
                | _ -> None
            ) }

    static member solveProblem problem =
        problem.Numbers
        |> Seq.skip 1
        |> Seq.fold
            problem.Operation
            (Seq.head problem.Numbers)

let inputPartTwo =
    let columns =
        "input.txt"
        |> File.ReadAllLines
        |> Seq.transpose

    let problems =
        columns |> Seq.fold
            (fun (state: char seq list list) column ->
                let isEndOfProblem = column |> Seq.forall ((=) ' ')
                if isEndOfProblem then
                    [] :: state
                else
                    (column :: List.head state) :: List.tail state
            )
            [ [] ]

    problems
    |> Seq.map Problem.fromStrings
    |> Seq.sumBy Problem.solveProblem