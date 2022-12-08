module Day8

type Matrix<'a> = 'a list list

let inline charToInt c = int c - int '0'

let treeMatrix: int Matrix =
    System.IO.File.ReadAllLines "./Inputs/Day8.txt"
    |> List.ofArray
    |> List.map Seq.toList
    |> List.map (List.map charToInt)

let getMatrixColumn index (matrix: _ Matrix) =
    matrix |> List.map (fun line -> line.[index])

let isTreeVisible (lineIndex: int) (columnIndex: int) treeMatrix =
    let column = treeMatrix |> getMatrixColumn columnIndex
    let line = treeMatrix |> List.item lineIndex
    let tree = line |> List.item columnIndex

    let lineLeft, lineRight =
        line |> List.splitAt columnIndex |> fst,
        line |> List.splitAt (columnIndex + 1) |> snd

    let columnTop, columnBottom =
        column |> List.splitAt lineIndex |> fst,
        column |> List.splitAt (lineIndex + 1) |> snd

    lineLeft |> List.filter (fun x -> x >= tree) |> List.isEmpty
    || lineRight |> List.filter (fun x -> x >= tree) |> List.isEmpty
    || columnTop |> List.filter (fun x -> x >= tree) |> List.isEmpty
    || columnBottom |> List.filter (fun x -> x >= tree) |> List.isEmpty

let getTreeScenicScore (lineIndex: int) (columnIndex: int) treeMatrix =
    let column = treeMatrix |> getMatrixColumn columnIndex
    let line = treeMatrix |> List.item lineIndex
    let tree = line |> List.item columnIndex

    let lineLeft, lineRight =
        line |> List.splitAt columnIndex |> fst,
        line |> List.splitAt (columnIndex + 1) |> snd

    let columnTop, columnBottom =
        column |> List.splitAt lineIndex |> fst,
        column |> List.splitAt (lineIndex + 1) |> snd

    [ lineLeft |> List.rev
      lineRight
      columnTop |> List.rev
      columnBottom ]
    |> List.map (List.takeWhileInclusive (fun x -> x < tree))
    |> List.map List.length
    |> List.reduce (*)


let part1 =
    treeMatrix
    |> List.mapi (fun lineIndex treeLine ->
        treeLine |> List.mapi (fun columnIndex _ ->
            isTreeVisible lineIndex columnIndex treeMatrix
        )
    )
    |> List.concat
    |> List.filter id
    |> List.length

let part2 =
    treeMatrix
    |> List.mapi (fun lineIndex treeLine ->
        treeLine |> List.mapi (fun columnIndex _ ->
            getTreeScenicScore lineIndex columnIndex treeMatrix
        )
    )
    |> List.concat
    |> List.max

printfn "Day 8 -> Part 1: %A" part1
printfn "Day 8 -> Part 2: %A" part2