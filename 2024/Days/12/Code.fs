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
module Pos =
    /// 2D cross product of OA and OB
    let cross o a b =
        (fst a - fst o)*(snd b - snd o)
        - (snd a - snd o)*(fst b - fst o)

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

    /// Same as counting face
    let countFaces (group: #seq<Pos>) =
        let getLineFaces useX line =
            let getCoordinate = if useX then snd else fst

            line
            |> Seq.pairwise
            |> Seq.fold
                (fun faces (left, right) ->
                    let xLeft, xRight =
                        left |> getCoordinate,
                        right |> getCoordinate

                    match xRight - xLeft with
                    | 1 -> faces
                    | _ -> [xLeft+1; xRight] @ faces
                )
                List.empty
            |> fun faces ->
                [ line |> Seq.head |> getCoordinate
                  line |> Seq.last |> getCoordinate |> (+) 1 ]
                @ faces

        let verticalFaces =
            group
            |> getLines
            |> Seq.indexed
            |> Seq.collect (fun (idx, line) ->
                line
                |> getLineFaces true
                |> List.map (fun faceIdx -> idx, faceIdx)
            ) // All face parts
            |> Seq.groupBy snd // Group them by columnIdx
            |> Seq.toList // TODO
            |> Seq.collect (fun (_faceIdx, faceParts) ->
                faceParts // Face parts that are in the same column
                |> Seq.fold
                    (fun faces facePart ->
                        let faceLineIdx = facePart |> fst

                        match faces |> List.tryHead with
                        | None -> [ [ facePart ] ]
                        | Some prevFace ->
                            let (prevFaceLineIdx, _) = prevFace |> List.head
                            match prevFaceLineIdx + 1 = faceLineIdx with
                            | true -> (facePart::prevFace) :: (faces |> List.tail)
                            | false -> [ facePart ] :: faces
                    )
                    []
            ) // Faces (Face parts grouped by face)
            |> Seq.length

        // let group =
        //     getInput()
        //     |> groups
        //     |> fun x -> x[2]
        //     |> snd
        //     |> Seq.toList

        let horizontalFaces =
            group
            |> getColumns
            |> Seq.indexed
            |> Seq.collect (fun (idx, line) ->
                line
                |> getLineFaces false
                |> List.map (fun faceIdx -> idx, faceIdx)
            ) // All face parts
            |> Seq.groupBy snd // Group them by columnIdx
            |> Seq.collect (fun (_faceIdx, faceParts) ->
                faceParts // Face parts that are in the same column
                |> Seq.fold
                    (fun faces facePart ->
                        let faceLineIdx = facePart |> fst

                        match faces |> List.tryHead with
                        | None -> [ [ facePart ] ]
                        | Some prevFace ->
                            let (prevFaceLineIdx, _) = prevFace |> List.head
                            match prevFaceLineIdx + 1 = faceLineIdx with
                            | true -> (facePart::prevFace) :: (faces |> List.tail)
                            | false -> [ facePart ] :: faces
                    )
                    []
            ) // Faces (Face parts grouped by face)
            |> Seq.length

        verticalFaces + horizontalFaces

let partOne =
    let getGroupPrice group = (group |> Seq.length) * (group |> Group.calculatePerimeter)

    getInput ()
    |> groups
    |> Seq.sumBy (_.Value >> snd >> getGroupPrice)

let partTwo = // Does not work...
    let getGroupPrice group = (group |> Seq.length) * (group |> Group.countFaces)

    getInput ()
    |> groups
    // |> Seq.map (fun group ->
    //     group.Value |> fst,
    //     group.Value
    //     |> snd
    //     |> getGroupPrice
    // )
    // |> Seq.iter (fun (x, y) -> printfn "%c -> %i" x y)
    |> Seq.sumBy (_.Value >> snd >> getGroupPrice)

