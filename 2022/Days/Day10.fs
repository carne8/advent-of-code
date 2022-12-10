module Day10

type Operation =
    | AddX of int // Take 2 cycles
    | Noop // Take 1 cycle

let input =
    "./Inputs/Day10.txt"
    |> System.IO.File.ReadAllLines
    |> List.ofArray
    |> List.map (String.split " ")
    |> List.map (fun line ->
        match line.[0] with
        | "addx" -> line.[1] |> int |> AddX
        | _ -> Noop)

let operatedX =
    input |> List.fold
        (fun (list: int list) newOperation ->
            let currentX = list |> List.last

            match newOperation with
            | Noop -> [ currentX ]
            | AddX amount -> [ currentX; currentX + amount ]
            |> List.append list)
        [ 1 ]


// ----------- Part 1 -------------

let getSignalStrength cycle =
    operatedX
    |> List.item (cycle - 1)
    |> (*) cycle

printfn "Day 10 -> Part 1: %A"
    ([ getSignalStrength 20
       getSignalStrength 60
       getSignalStrength 100
       getSignalStrength 140
       getSignalStrength 180
       getSignalStrength 220 ]
     |> List.sum)


// ----------- Part 2 -------------

printfn "Day 10 -> Part 1:"

operatedX
|> List.indexed
|> List.fold
    (fun (crt: bool list) (index, spritePosition) ->
        [ spritePosition - 1; spritePosition; spritePosition + 1 ]
        |> List.contains (index % 40)
        |> List.singleton
        |> List.append crt
    )
    []
|> List.map (function | true -> "#" | false -> ".")
|> List.chunkBySize 40
|> List.map (String.concat "")
|> List.iter (printfn "  %s")