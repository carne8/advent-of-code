module AdventOfCode._2024._23

open System.IO
open System.Collections.Generic

let getInput () =
    "input.txt"
    |> File.ReadAllLines
    |> Array.fold
        (fun (computers, connections: List<string * string>) line ->
            match line.Split "-" with
            | [| c1; c2 |] ->
                connections.Add (c1, c2)

                computers
                |> Set.add c1
                |> Set.add c2,
                connections
            | _ -> failwithf "Incorrect line: %A" line
        )
        (Set.empty, new List<_>())

let buildConnectionMap connections =
    let connectionMap = Dictionary<string, List<string>>()

    for connection in connections do
        let c1, c2 = connection

        match connectionMap.TryGetValue c1 with
        | false, _ -> connectionMap.Add(c1, List<_>([ c2 ]))
        | true, _ -> connectionMap[c1].Add c2

        match connectionMap.TryGetValue c2 with
        | false, _ -> connectionMap.Add(c2, List<_>([ c1 ]))
        | true, _ -> connectionMap[c2].Add c1

    connectionMap
    |> Seq.map (fun kv -> kv.Key, set kv.Value)
    |> dict

let partOne () =
    let _computers, connections = getInput()
    let connectionMap = connections |> buildConnectionMap

    let getTrios c1 =
        connectionMap[c1]
        |> Seq.collect (fun c2 ->
            let commonConnections =
                Set.intersect
                    connectionMap[c1]
                    connectionMap[c2]

            commonConnections
            |> Seq.map (fun c3 -> set [ c1; c2; c3 ])
        )

    connectionMap
    |> Seq.collect (_.Value >> Seq.collect getTrios) // For all computer, find trios from connected computers
    |> Seq.distinct // List of distinct trios
    |> Seq.filter (Seq.exists (fun c -> c.StartsWith "t"))
    |> Seq.length

let partTwoTry () = // Doesn't work
    let computers, connections = getInput()
    let connectionMap = connections |> buildConnectionMap

    let mutable i = computers.Count - 1
    let mutable group: Set<string> option = None

    while group.IsNone && i > 2 do
        let c =
            connectionMap
            |> Seq.collect (fun kv ->
                let c, connections = kv.Deconstruct()

                connections
                |> Seq.windowed i
                |> Seq.map (fun connections -> c, set connections)
            )

        c
        |> Seq.choose (fun (c, connections) ->
            // printfn "--- %A" c
            // let (c, connections) = c.Value
            let allConnectedToEachOther =
                connections
                |> Seq.tryFind (fun c ->
                    connections
                    |> Set.remove c
                    |> Seq.forall connectionMap[c].Contains
                    |> not
                )
                |> Option.isNone

            match allConnectedToEachOther with
            | false -> None
            | true -> connections |> Set.add c |> Some
        )
        |> fun x ->
            printfn "aaa"
            x
            |> set
            |> Seq.iter (Seq.reduce (sprintf "%s,%s") >> printfn "%s")
            x |> Seq.tryHead
        |> function
            | None -> i <- i-1
            | Some g -> group <- Some g

// Uses the Bron-Kerbosch algorithm with pivot
// https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm#With_pivoting
let partTwo () =
    let computers, connections = getInput()
    let connectionMap = connections |> buildConnectionMap

    let mutable maxClique = Set.empty

    let rec bronKerbosch currentClique candidates excluded =
        match candidates = Set.empty && excluded = Set.empty with
        | true ->
            if (currentClique |> Set.count) > maxClique.Count then
                maxClique <- currentClique
            else
                ()

        | false ->
            // Pivot is took arbitrarily
            let pivot = Set.union candidates excluded |> Set.maxElement
            let pivotNeighbors = connectionMap[pivot]

            Set.difference candidates pivotNeighbors
            |> Seq.fold
                (fun (p, x) v ->
                    bronKerbosch
                        (currentClique |> Set.add v)
                        (Set.intersect p connectionMap[v])
                        (Set.intersect x connectionMap[v])
                    p |> Set.add v,
                    x |> Set.remove v
                )
                (candidates, excluded)
            |> ignore

    bronKerbosch Set.empty computers Set.empty
    maxClique |> Seq.reduce (sprintf "%s,%s")