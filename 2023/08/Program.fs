open System.IO

let input = File.ReadAllLines "./input.txt"
let testInput = File.ReadAllLines "./test-input.txt"
let testInputPart2 = File.ReadAllLines "./test-input-two.txt"

type Node = { Name: string; Left: string; Right: string }

let parseInstructions input =
    input
    |> Array.head
    |> Array.ofSeq

let parseNodes (input: string array) =
    input
    |> Array.skip 2
    |> Array.map (fun line ->
        { Name = line.Substring(0, 3)
          Left = line.Substring(7, 3)
          Right = line.Substring(12, 3) }
    )


let part1 =
    let instructions = input |> parseInstructions
    let nodes = input |> parseNodes

    let mutable i = 0
    let mutable currentNode = nodes |> Array.find (_.Name >> (=) "AAA")

    while currentNode.Name <> "ZZZ" do
        let currentInstruction =
            instructions |> Array.item (i % instructions.Length)

        let nextNode =
            match currentInstruction with
            | 'L' -> nodes |> Array.find (_.Name >> (=) currentNode.Left)
            | 'R' -> nodes |> Array.find (_.Name >> (=) currentNode.Right)
            | _ -> failwith "Unknown instruction"

        currentNode <- nextNode
        i <- i + 1

    i

printfn "Part 1: %A" part1

// --- Part 2 ---
let isStartNode node = node.Name.EndsWith "A"
let isEndNode node = node.Name.EndsWith "Z"

let part2 =
    let instructions = input |> parseInstructions
    let nodes = input |> parseNodes

    let mutable i = 0
    let mutable currentNodes = nodes |> Array.filter isStartNode

    let struct (x, y) = System.Console.GetCursorPosition()

    while currentNodes |> Array.forall isEndNode |> not do
        let currentInstruction =
            instructions |> Array.item (i % instructions.Length)

        // printfn "--- "

        let nextNodes =
            currentNodes
            |> Array.map (fun node ->
                match currentInstruction with
                | 'L' -> nodes |> Array.find (_.Name >> (=) node.Left)
                | 'R' -> nodes |> Array.find (_.Name >> (=) node.Right)
                | _ -> failwith "Unknown instruction"
                // |> fun x ->
                //     printfn "Node %s -> %c -> %s" node.Name currentInstruction x.Name
                //     x
            )

        currentNodes <- nextNodes
        i <- i + 1

        System.Console.SetCursorPosition(x, y)
        System.Console.Write i

    i

printfn "Part 2: %A" part2