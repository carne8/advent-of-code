open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllText "./input.txt"

type MapRange =
    { SourceStart: int64
      DestinationStart: int64
      Length: int64 }

    static member parse (str: string) =
        let numbers = str.Split " "
        { SourceStart = numbers |> Array.item 1 |> int64
          DestinationStart = numbers |> Array.item 0 |> int64
          Length = numbers |> Array.item 2 |> int64 }

    static member getFun range x =
        match range.SourceStart <= x && x < range.SourceStart + range.Length with
        | false -> None
        | true ->
            x - (range.SourceStart - range.DestinationStart)
            |> Some

type Type =
    | Seed
    | Soil
    | Fertilizer
    | Water
    | Light
    | Temperature
    | Humidity
    | Location

    static member parse =
        function
        | "seed" -> Seed
        | "soil" -> Soil
        | "fertilizer" -> Fertilizer
        | "water" -> Water
        | "light" -> Light
        | "temperature" -> Temperature
        | "humidity" -> Humidity
        | "location" -> Location
        | other -> failwithf "Unknown type name: %s" other

type Map =
    { Source: Type; Destination: Type; Ranges: MapRange array }

    static member parse (str: string) =
        let lines = str.Split "\n"
        let fstType, sndType =
            lines
            |> Array.head // Map title
            |> fun str -> str.Split " "
            |> Array.head // Map name
            |> fun str -> str.Split "-to-"
            |> function
                | [| fstType; sndType |] -> fstType |> Type.parse, sndType |> Type.parse
                | other -> failwithf "Failed to parse map name: %A" other

        let ranges =
            lines
            |> Array.tail
            |> Array.map MapRange.parse

        { Source = fstType
          Destination = sndType
          Ranges = ranges }

    static member toFun map numberToConvert =
        map.Ranges
        |> Array.fold
            (fun convertedNumber range ->
                match convertedNumber with
                | Some x -> Some x
                | None -> numberToConvert |> (range |> MapRange.getFun)
            )
            None
        |> Option.defaultValue numberToConvert

let part1 =
    let rawMaps = input.ReplaceLineEndings().Split "\r\n\r\n"
    let seeds =
        rawMaps
        |> Array.head
        |> fun str -> str.Substring(7).Split " "
        |> Array.map int64

    let maps =
        rawMaps
        |> Array.tail
        |> Array.map Map.parse

    let pipeline =
        (maps |> Array.find (_.Destination >> (=) Soil) |> Map.toFun)
        >> (maps |> Array.find (_.Source >> (=) Soil) |> Map.toFun)
        >> (maps |> Array.find (_.Source >> (=) Fertilizer) |> Map.toFun)
        >> (maps |> Array.find (_.Source >> (=) Water) |> Map.toFun)
        >> (maps |> Array.find (_.Source >> (=) Light) |> Map.toFun)
        >> (maps |> Array.find (_.Source >> (=) Temperature) |> Map.toFun)
        >> (maps |> Array.find (_.Source >> (=) Humidity) |> Map.toFun)

    seeds
    |> Array.map pipeline
    |> Array.min

printfn "Part 1: %A" part1