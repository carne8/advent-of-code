module Day5

open System.Collections.Generic

type Instruction = { Quantity: int; From: int; Destination: int }

let input = System.IO.File.ReadLines "./Inputs/Day5.txt" |> List.ofSeq

let rawStacks, rawInstructions =
    input
    |> List.findIndex (fun x -> x = "")
    |> fun separatorIndex ->
        input |> List.splitAt separatorIndex

let parsedStacks =
    rawStacks
    |> List.except [ rawStacks |> List.last ]
    |> List.map (Seq.chunkBySize 4 >> Seq.map (Seq.item 1))

let stacks =
    let totalStacks =
        parsedStacks
        |> List.map Seq.length
        |> List.max

    [0..totalStacks - 1]
    |> List.map (fun i ->
        parsedStacks
        |> List.collect
            (Seq.indexed
             >> Seq.filter (fun (index, crate) -> crate <> ' ' && index = i)
             >> Seq.map snd
             >> Seq.toList)
    )
    |> List.map (List.rev >> Stack)

let parsedInstructions =
    rawInstructions
    |> List.tail
    |> List.map (String.split " ")
    |> List.map (fun x -> [ x.[1]; x.[3]; x.[5] ])
    |> List.map (List.map int)
    |> List.map (fun x -> { Quantity = x.[0]; From = x.[1]; Destination = x.[2] })

let getTopCrates (stack: char Stack list) =
    stack
    |> List.map (fun stack -> stack.TryPeek())
    |> List.map (fun (found, item) -> if found then Some item else None)
    |> List.map (Option.defaultValue ' ')
    |> String.fromChars


let part1 =
    let stacks = List.init stacks.Length (fun i -> stacks.[i] |> Seq.rev |> Stack)

    parsedInstructions
    |> List.iter (fun inst ->
        let sourceStack = stacks |> List.item (inst.From - 1)
        let targetStack = stacks |> List.item (inst.Destination - 1)

        for _ in [1..inst.Quantity] do
            targetStack.Push(sourceStack.Pop())
    )

    stacks |> getTopCrates

let part2 =
    let stacks = List.init stacks.Length (fun i -> stacks.[i] |> Seq.rev |> Stack)

    parsedInstructions
    |> List.iter (fun inst ->
        let sourceStack = stacks |> List.item (inst.From - 1)
        let targetStack = stacks |> List.item (inst.Destination - 1)

        let elements =
            List.init
                inst.Quantity
                (fun _ -> sourceStack.Pop())

        elements
        |> List.rev
        |> List.iter targetStack.Push
    )

    stacks |> getTopCrates


printfn "Day 5 -> Part 1 -> %A" part1
printfn "Day 5 -> Part 2 -> %A" part2