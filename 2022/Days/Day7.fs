module Day7

open System.IO

let input =
    File.ReadAllLines "./2022/Inputs/Day7.txt"
    |> List.ofArray

type Command =
    | CD of string
    | LS

    static member FromString =
        String.splitc ' '
        >> List.ofArray
        >> function
            | "$" :: "cd" :: args -> CD args.[0]
            | _ -> LS

let commands =
    let mutable lastCmdIndex = 0

    input
    |> List.groupBy (fun line ->
        if line.[0] = '$' then
            lastCmdIndex <- lastCmdIndex + 1

        lastCmdIndex)
    |> List.map snd
    |> List.map (fun command -> command |> List.head |> Command.FromString, command |> List.tail)


let baseDirectory = __SOURCE_DIRECTORY__ + "\\Day7-temp\\"

let createFiles () =
    commands
    |> List.fold
        (fun currentDir (command, result) ->
            match command with
            | CD "/" -> baseDirectory
            | CD x -> sprintf "%s%s\\" currentDir x
            | LS ->
                result
                |> List.iter (
                    String.split " "
                    >> List.ofArray
                    >> function
                        | "dir" :: folderName ->
                            currentDir + folderName.[0]
                            |> Directory.CreateDirectory
                            |> ignore
                        | fileSize :: fileName ->
                            sprintf "%s%s-%s" currentDir fileSize fileName.[0]
                            |> File.Create
                            |> ignore
                        | [] -> failwith "not expected"
                )

                currentDir)
        baseDirectory
    |> ignore
    baseDirectory


let getFileSize = String.split "-" >> Array.head >> int

let rec getDirectorySize dir =
    let filesSize =
        dir
        |> Directory.GetFiles
        |> Array.map Path.GetFileName
        |> Array.map getFileSize
        |> Array.sum

    dir
    |> Directory.GetDirectories
    |> function
        | [||] -> filesSize
        | subDirs ->
            subDirs
            |> Array.map getDirectorySize
            |> Array.sum
            |> (+) filesSize


let rec getSubDirectories baseDir : string list =
    baseDir
    |> Directory.GetDirectories
    |> List.ofArray
    |> function
        | [] -> []
        | subDirs -> subDirs @ (subDirs |> List.collect getSubDirectories)

let part1 workDir =
    workDir
    |> getSubDirectories
    |> List.map (fun dir -> dir, getDirectorySize dir)
    |> List.map snd
    |> List.filter ((>) 100000)
    |> List.sum

let part2 workDir =
    let totalSpace = 70000000
    let requiredSpace = 30000000

    let usedSpace = workDir |> getDirectorySize
    let unusedSpace = totalSpace - usedSpace

    let spaceToFind = requiredSpace - unusedSpace

    workDir
    |> getSubDirectories
    |> List.append [ workDir ]
    |> List.map getDirectorySize
    |> List.sort
    |> List.find ((<=) spaceToFind)

let workDir = createFiles()
printfn "Day 7 -> Part 1: %A" (part1 workDir)
printfn "Day 7 -> Part 2: %A" (part2 workDir)