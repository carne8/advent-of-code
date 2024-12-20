module AdventOfCode._2024._12

open System.IO
open System.Collections.Generic

let getInput () =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (fun s -> s.ToCharArray())
    |> array2D

module Array2D =
    let tryItem y x (arr: _ array2d) =
        try
            arr[y, x] |> Some
        with _ -> None

type Pos = int * int

let groups input =
    let groups = new Dictionary<int, (char * List<Pos>)>()
    let chars = new Dictionary<Pos, int>()
    let mutable groupId = 0

    for y in 0..(input |> Array2D.length1)-1 do
        for x in 0..(input |> Array2D.length2)-1 do
            let alreadyInGroup = chars.ContainsKey (y, x)
            let char = input[y, x]
            let currentGroupId, group =
                match alreadyInGroup with
                | true ->
                    let groupId = chars[y, x]
                    groupId, groups[groupId] |> snd
                | false -> groupId, List()

            if not alreadyInGroup then group.Add (y, x)
            let mutable groupIdToMerge: int option = None
            let mutable groupToMerge: List<Pos> option = None

            // Add right
            let right = input |> Array2D.tryItem y (x+1)
            if right.IsSome && right.Value = char then
                if chars.ContainsKey (y, x+1) then // Does the right char is in a group ?
                    // Is his group the same has mine ?
                    let charGroupId = chars[y, x+1]

                    if charGroupId <> currentGroupId then // If not merge groups
                        groupIdToMerge <- Some charGroupId
                        groupToMerge <- groups[charGroupId] |> snd |> Some

                else group.Add (y, x+1)

            // Add bottom
            let bottom = input |> Array2D.tryItem (y+1) x
            match bottom with
            | Some c when c = char ->
                // Don't check if it is already in a group because it is impossible
                group.Add (y+1, x)
            | _ -> ()

            // Mark chars as "in group"
            let targetGroupId = groupIdToMerge |> Option.defaultValue currentGroupId
            group |> Seq.iter (fun pos ->
                if chars.TryAdd(pos, targetGroupId) |> not then
                    chars.Remove(pos) |> ignore
                    chars.Add(pos, targetGroupId)
            )

            match groupToMerge with
            | Some groupToMerge ->
                groupToMerge.AddRange group
                if alreadyInGroup then groups.Remove(currentGroupId) |> ignore

            | None ->
                if not alreadyInGroup then
                    groups.Add(groupId, (char, group))
                    groupId <- groupId+1

    groups

module Group =
    let getLines (group: #seq<Pos>) =
        group
        |> Seq.groupBy fst
        |> Seq.sortBy fst
        |> Seq.map (snd >> Seq.sortBy snd)

    let getColumns (group: #seq<Pos>) =
        group
        |> Seq.groupBy snd
        |> Seq.sortBy fst
        |> Seq.map (snd >> Seq.sortBy fst)

    let calculatePerimeter (group: #seq<Pos>) =
        let getLinePerimeter useX =
            Seq.pairwise >> Seq.fold
                (fun acc (left, right) ->
                    let xLeft, xRight =
                        match useX with
                        | true -> left |> snd, right |> snd
                        | false -> left |> fst, right |> fst

                    match xRight - xLeft with
                    | 1 -> acc
                    | _ -> acc + 2
                )
                2

        let horizontalPerimeter =
            group
            |> getLines
            |> Seq.map (getLinePerimeter true)
            |> Seq.sum

        let verticalPerimeter =
            group
            |> getColumns
            |> Seq.map (getLinePerimeter false)
            |> Seq.sum

        horizontalPerimeter + verticalPerimeter

    /// Same as counting faces
    let countVertexes (group: #seq<Pos>) =
        let minY = group |> Seq.minBy fst |> fst
        let minX = group |> Seq.minBy snd |> snd
        let groupHeight = (group |> Seq.maxBy fst |> fst) - minY + 1
        let groupWidth = (group |> Seq.maxBy snd |> snd) - minX + 1

        let grid =
            Array2D.init groupHeight groupWidth (fun y x ->
                group |> Seq.tryFind ((=) (y + minY, x + minX))
            )

        let mutable vertexesCount = 0

        for y in -1..(grid |> Array2D.length1) do
            for x in -1..(grid |> Array2D.length2) do
                let detectionBlocks =
                    [ 0, 0 // Top left
                      0, 1 // Top right
                      1, 0 // Bottom left
                      1, 1 (* Bottom right *) ]
                    |> List.map (fun (y', x') ->
                        grid
                        |> Array2D.tryItem (y+y') (x+x')
                        |> Option.bind id
                        |> Option.isSome
                    )

                match detectionBlocks with
                | _ when detectionBlocks |> List.filter id |> List.length = 1 ->
                    vertexesCount <- vertexesCount+1
                | _ when detectionBlocks |> List.filter (id >> not) |> List.length = 1 ->
                    vertexesCount <- vertexesCount+1
                | [ true; false; false; true ]
                | [ false; true; true; false ] -> vertexesCount <- vertexesCount+2
                | _ -> ()

        vertexesCount

let partOne () =
    let getGroupPrice group = // Area * Perimeter
        (group |> Seq.length) * (group |> Group.calculatePerimeter)

    getInput ()
    |> groups
    |> Seq.sumBy (_.Value >> snd >> getGroupPrice)

let partTwo () =
    let getGroupPrice group = // Area * Faces count
        (group |> Seq.length) * (group |> Group.countVertexes)

    getInput ()
    |> groups
    |> Seq.sumBy (_.Value >> snd >> getGroupPrice)
