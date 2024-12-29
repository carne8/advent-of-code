module AdventOfCode._2024._24

open System.IO
open System.Collections.Generic
// #r "nuget: FsToolkit.ErrorHandling, 4.18.0"
open FsToolkit.ErrorHandling

type Connection =
    { WireOne: string
      WireTwo: string
      Gate: string }

let gates =
    [ "AND", (&&&)
      "OR", (|||)
      "XOR", (^^^) ]
    |> dict

let getInput () =
    let txt = "input.txt" |> File.ReadAllText

    match txt.Split "\n\n" with
    | [| initialWireValues; connections |] ->
        let initialValues =
            initialWireValues.Split "\n"
            |> Array.map (fun l ->
                match l.Split ": " with
                | [| wireName; wireValue |] -> wireName, byte wireValue
                | _ -> failwithf "Incorrect wire init: %A" l
            )
            |> dict

        let connections =
            connections.Split "\n"
            |> Array.map (fun l ->
                match l.Split " " with
                | [| wireOne; gate; wireTwo; _; outputWire |] ->
                    outputWire,
                    { WireOne = wireOne
                      WireTwo = wireTwo
                      Gate = gate }
                | _ -> failwithf "Incorrect wire connection: %A" l
            )
            |> dict

        initialValues, connections
    | _ -> failwith "Invalid input"

let getWiresValues (initialValues: IDictionary<_, _>) (connections: IDictionary<_, _>) =
    let wireValues = new Dictionary<string, byte>(initialValues)
    let rec getWireValue wireName =
        match wireValues.TryGetValue(wireName) with
        | true, v -> v
        | false, _ ->
            let conn = connections[wireName]

            gates[conn.Gate]
                (conn.WireOne |> getWireValue)
                (conn.WireTwo |> getWireValue)
            |> fun v ->
                wireValues.Add(wireName, v)
                v

    connections
    |> Seq.filter (_.Key >> fun s -> s.StartsWith "z")
    |> Seq.iter (_.Key >> fun wire -> wire |> getWireValue |> ignore)

    wireValues

let partOne () =
    let initialValues, connections = getInput()

    getWiresValues initialValues connections
    |> Seq.map (fun kv -> kv.Key, kv.Value)
    |> Seq.sumBy (fun (wire, value) ->
        match wire[0] with
        | 'z' ->
            let bitIdx = wire.Substring(1) |> int
            int64 value <<< bitIdx
        | _ -> 0
    )

let partTwo () =
    let _initialWireValues, connections = getInput()

    let getWireBitIdx (wire: string) = wire.Substring(1) |> int
    let mostSignificantBit =
        connections
        |> Seq.fold
            (fun (maxWire, maxBitIdx) kv ->
                let wire = kv.Key
                try
                    let bitIdx = wire |> getWireBitIdx
                    match bitIdx > maxBitIdx with
                    | true -> wire, bitIdx
                    | false -> maxWire, maxBitIdx
                with _ ->
                    // printfn "%A" wire
                    maxWire, maxBitIdx
            )
            ("z00", 0)
        |> snd

    let incorrectWires =
        connections
        |> Seq.choose (fun kv ->
            let wire = kv.Key
            let conn = kv.Value

            result {
                do! match wire[0], conn.Gate with
                    | 'z', "XOR" -> Ok ()
                    | 'z', _ when wire |> getWireBitIdx = mostSignificantBit -> Ok ()
                    | 'z', _ -> Error wire
                    | _ -> Ok ()

                do! match conn.Gate, conn.WireOne[0], conn.WireTwo[0], wire[0] with
                    | "XOR", 'x', 'y', _ -> Ok ()
                    | "XOR", 'y', 'x', _ -> Ok ()
                    | "XOR", _, _, 'z' -> Ok ()
                    | "XOR", _, _, _ ->
                        printfn "%A" wire
                        Error wire
                    | _ -> Ok ()

                // Find "AND" wires that are connected to non "OR" wires
                do! match conn.Gate with
                    | "AND" when conn.WireOne.Substring(1) <> "00" ->
                        let inputOfNonOrGate =
                            connections |> Seq.exists (
                                _.Value
                                >> fun c ->
                                    (c.WireOne = wire || c.WireTwo = wire)
                                    && c.Gate <> "OR"
                            )

                        if inputOfNonOrGate then
                            Error wire
                        else
                            Ok ()
                    | _ -> Ok ()

                // Find "XOR" wires that are connected to "OR" wires
                do! match conn.Gate with
                    | "XOR" ->
                        let inputOfToOrGate =
                            connections |> Seq.exists (
                                _.Value
                                >> fun c ->
                                    (c.WireOne = wire || c.WireTwo = wire)
                                    && c.Gate = "OR"
                            )

                        if inputOfToOrGate then
                            Error wire
                        else
                            Ok ()
                    | _ -> Ok ()
            }
            |> function
                | Ok _ -> None
                | Error wire -> Some wire
        )
        |> Set.ofSeq

    incorrectWires |> Seq.reduce (sprintf "%s,%s")