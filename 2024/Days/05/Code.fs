module AdventOfCode._2024._05

open System
open System.IO

let parseUpdate (update: string) = update.Split(",") |> Array.map int

let orderingRules, updates =
    "input.txt"
    |> File.ReadAllText
    |> fun s -> s.Split("\n\n")
    |> function
        | [| x; y |] -> x, y
        | _ -> failwith "Failed to open input"
    |> fun (orderingRules, updates) ->
        orderingRules.Split("\n")
        |> Array.map (fun line ->
            line.Split("|")
            |> function
                | [| a; b |] -> (int a, int b)
                | _ -> failwith "Failed to parse rule"
        ),
        updates.Split("\n")
        |> Array.map parseUpdate

let getUpdateOrderingErrors (orderingRules: (int * int) array) (update: int array) =
    orderingRules
    |> Array.filter (fun rule ->
        let a, b = rule

        let mutable correct = true
        let mutable skip = false
        let mutable pageIdx = 0
        let mutable foundB = false

        while correct && not skip && pageIdx < update.Length do
            let page = update |> Array.item pageIdx

            // printfn "Check page: %A -> %b, %b" page (page = a) foundB
            match page = a, foundB with
            | true, true -> // Found a after b
                correct <- false
            | true, false -> // Found a before b
                pageIdx <- pageIdx+1
            | false, true -> // Found b before a (maybe there isn't a)
                if page = b then foundB <- true
                pageIdx <- pageIdx+1
            | false, false -> // Did not find a nor b
                if page = b then foundB <- true
                pageIdx <- pageIdx+1

        // match correct with
        // | false ->
        //     printfn "%A is not correct ! Break rule: %A" update rule
        //     correct
        // | true -> correct
        not correct
    )

let fixUpdate (orderingRules: (int * int) array) baseUpdate =
    let mutable update = baseUpdate |> Array.copy
    let mutable errors = update |> getUpdateOrderingErrors orderingRules
    let correct () = errors |> Array.isEmpty
    let needToChange = errors |> Array.isEmpty |> not

    while correct() |> not do
        let a, b = errors |> Array.head
        let aIdx = update |> Array.findIndex ((=) a)
        let bIdx = update |> Array.findIndex ((=) b)

        update <-
            update |> Array.permute (fun idx ->
                if idx = aIdx then bIdx
                elif idx = bIdx then aIdx
                else idx
            )

        errors <- update |> getUpdateOrderingErrors orderingRules

    (needToChange, update)

let partOne =
    updates
    |> Array.filter (getUpdateOrderingErrors orderingRules >> Array.isEmpty)
    |> Array.map (fun update ->
        let centerIdx =
            (update.Length |> float) / 2.
            |> Math.Floor
            |> int

        update |> Array.item centerIdx
    )
    |> Array.sum

let partTwo =
    updates
    |> Array.indexed
    |> Array.choose (fun (i, update) ->
        let (hasBeenChanged, newUpdate) = update |> fixUpdate orderingRules
        match hasBeenChanged with
        | false -> None
        | true ->
            let centerIdx =
                (newUpdate.Length |> float) / 2.
                |> Math.Floor
                |> int

            newUpdate
            |> Array.item centerIdx
            |> Some
    )
    |> Array.sum