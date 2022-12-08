module Day8

type Matrix<'a> = 'a list list
module Matrix =
    let getColumn index (matrix: _ Matrix) =
        matrix |> List.map (fun line -> line.[index])

    let map f (matrix: _ Matrix) : _ Matrix = matrix |> List.map (List.map f)

    let locateAllElements (matrix: _ Matrix) : _ Matrix =
        matrix
        |> List.mapi (fun lineIndex line ->
            line
            |> List.mapi (fun columnIndex element -> element, (lineIndex, columnIndex)))

let inline charToInt c = int c - int '0'

let treeMatrix =
    System.IO.File.ReadAllLines "./Inputs/Day8.txt"
    |> List.ofArray
    |> List.map Seq.toList
    |> List.map (List.map charToInt)


let getTreeCrossingLines (lineIndex: int, columnIndex: int) treeMatrix =
    let column = treeMatrix |> Matrix.getColumn columnIndex
    let line = treeMatrix |> List.item lineIndex
    let tree = line |> List.item columnIndex

    let lineLeft, lineRight =
        line |> List.splitAt columnIndex |> fst,
        line |> List.splitAt (columnIndex + 1) |> snd

    let columnTop, columnBottom =
        column |> List.splitAt lineIndex |> fst,
        column |> List.splitAt (lineIndex + 1) |> snd

    tree, lineLeft, lineRight, columnTop, columnBottom


let isTreeVisible (lineIndex: int, columnIndex: int) treeMatrix =
    let tree, lineLeft, lineRight, columnTop, columnBottom =
        getTreeCrossingLines (lineIndex, columnIndex) treeMatrix

    [ lineLeft
      lineRight
      columnTop
      columnBottom ]
    |> List.map (List.filter (fun x -> x >= tree)) // Check if all parts of line contain tallest trees than the current.
    |> List.map List.isEmpty // Check if there are parts of line that contain only trees smallest that the current.
    |> List.contains true

let getTreeScenicScore (lineIndex: int, columnIndex: int) treeMatrix =
    let tree, lineLeft, lineRight, columnTop, columnBottom =
        getTreeCrossingLines (lineIndex, columnIndex) treeMatrix

    [ lineLeft |> List.rev
      lineRight
      columnTop |> List.rev
      columnBottom ]
    |> List.map (List.takeWhileInclusive (fun x -> x < tree))
    |> List.map List.length
    |> List.reduce (*)


let part1 =
    treeMatrix
    |> Matrix.locateAllElements
    |> Matrix.map snd
    |> Matrix.map (fun pos -> isTreeVisible pos treeMatrix)
    |> List.concat
    |> List.filter id
    |> List.length

let part2 =
    treeMatrix
    |> Matrix.locateAllElements
    |> Matrix.map snd
    |> Matrix.map (fun pos -> getTreeScenicScore pos treeMatrix)
    |> List.concat
    |> List.max

printfn "Day 8 -> Part 1: %A" part1
printfn "Day 8 -> Part 2: %A" part2