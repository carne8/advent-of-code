module Day11

type Test =
    { Test: int -> bool
      IfTrue: int
      IfFalse: int }

type Monkey =
    { Items: int list
      Operation: int -> int
      Test: Test }

let monkeys =
    "./Inputs/Day11.txt"
    |> System.IO.File.ReadAllText
    |> String.split "\n\n" // Get monkey
    |> List.map (String.split "\n") // Get monkey props
    |> List.map List.tail
    |> List.map (List.map (String.split ":"))
    |> List.map (List.map List.last)
    |> List.map (fun monkeyProps ->
        let items =
            monkeyProps.[0]
            |> String.trim
            |> String.split ", "
            |> List.map int

        let operation =
            monkeyProps.[1]
            |> String.trim
            |> String.substring 10
            |> String.split " "
            |> fun x -> x.[0], x.[1]
            |> fun (op, x) ->
                match x with
                | "old" ->
                    match op with
                    | "+" -> (*) 2
                    | "*" -> fun (y: int) -> y * y
                    | _ -> failwith "Error"
                | x ->
                    match op with
                    | "+" -> (+) (int x)
                    | "*" -> fun y -> y * (int x)
                    | _ -> failwith "Error"

        let test =
            monkeyProps.[2]
            |> String.trim
            |> String.substring 13
            |> int
            |> fun divisor -> (fun dividend -> dividend % divisor = 0)

        let ifTrue =
            monkeyProps.[3]
            |> String.trim
            |> String.substring 16
            |> int

        let ifFalse =
            monkeyProps.[4]
            |> String.trim
            |> String.substring 16
            |> int

        { Items = items
          Operation = operation
          Test =
            { Test = test
              IfTrue = ifTrue
              IfFalse = ifFalse } })


module Item =
    let play monkey item =
        item
        |> monkey.Operation
        |> fun x -> (x |> float) / 3.
        |> System.Math.Floor
        |> int

    let getTargetMonkey monkey item =
        item
        |> monkey.Test.Test
        |> function
            | true -> monkey.Test.IfTrue
            | false -> monkey.Test.IfFalse

    let inspect monkey item =
        let newItem = item |> play monkey
        let targetMonkeyIndex = newItem |> getTargetMonkey monkey

        targetMonkeyIndex, newItem

module Monkey =
    let addItem item monkey = { monkey with Items = monkey.Items @ [ item ] }
    let emptyItems monkey = { monkey with Items = [] }

    let throwItems monkeys monkeyIdx monkey =
        monkey.Items
        |> List.fold
            (fun (monkeys: Monkey list, score) item ->
                let targetMonkeyIdx, item = item |> Item.inspect monkey
                let targetMonkey = monkeys.[targetMonkeyIdx]
                let newMonkey = targetMonkey |> addItem item

                monkeys
                |> List.replace
                    targetMonkeyIdx
                    newMonkey,
                score + 1
            )
            (monkeys, 0)
        |> fun (monkeys, score) ->
            monkeys
            |> List.replace
                monkeyIdx
                (monkey |> emptyItems),
            score

let playRound (monkeys: Monkey list) =
    monkeys
    |> List.indexed
    |> List.map fst
    |> List.fold
        (fun ((monkeys: Monkey list), scores) currentMonkeyIdx ->
            monkeys.[currentMonkeyIdx]
            |> Monkey.throwItems monkeys currentMonkeyIdx
            |> fun (monkeys, score) ->
                monkeys, scores @ [ score ]
        )
        (monkeys, [])
    |> fun (monkeys, scores) -> List.zip monkeys scores

let playSeveralRounds count monkeys =
    List.init count id
    |> List.fold
        (fun (monkeys, monkeyScores) _ ->
            monkeys
            |> playRound
            |> List.unzip
            |> fun (monkeys, scores) ->
                monkeys,
                List.map2
                    (+)
                    scores
                    monkeyScores
        )
        (monkeys, List.replicate monkeys.Length 0)


monkeys
|> playSeveralRounds 20
|> snd
|> List.sortDescending
|> List.indexed
|> List.filter (fun (i, _) -> i < 2)
|> List.map snd
|> List.reduce (*)
|> printfn "Day 11 -> Part 1: %A"