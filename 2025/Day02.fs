module AOC._2025.Day02

open System.IO

let input =
    "input.txt"
    |> File.ReadAllText
    |> _.Split(',')
    |> Array.map (fun pair ->
        match pair.Split '-' with
        | [| a; b |] -> int64 a, int64 b
        | _ -> failwith "Invalid range"
    )

let findSymmetric (start: int64) (end': int64) =
    seq { for i = start to end' do i }
    |> Seq.filter (fun number ->
        let str = string number
        match str.Length % 2 with
        | 1 -> false
        | _ -> str.Substring(str.Length / 2) = str.Substring(0, str.Length / 2)
    )

let partOne () =
    input
    |> Seq.collect (fun (a, b) -> findSymmetric a b)
    |> Seq.sum


let isNSym n (number: int) =
    let str = string number
    let u = str.Length / n
    let stringToMatch = str.Substring(0, u)

    seq {
        for i = 1 to n-1 do
            stringToMatch = str.Substring(u * i, u)
    }
    |> Seq.forall id


let findAllSymmetric (start: int64) (end': int64) =
    seq { for i = start to end' do i }
    |> Seq.filter (fun number ->
        let str = string number

        seq {
            for n = 2 to str.Length do
                if str.Length % n = 0 then int n
        }
        |> Seq.exists (fun divisor ->
            let u = str.Length / divisor
            let stringToMatch = str.Substring(0, u)

            seq {
                for i = 1 to divisor-1 do
                    stringToMatch = str.Substring(u * i, u)
            }
            |> Seq.exists ((=) false)
            |> not
        )
    )
    |> Seq.toList

let partTwo () =
    input
    |> Seq.collect (fun (a, b) -> findAllSymmetric a b)
    |> Seq.sum