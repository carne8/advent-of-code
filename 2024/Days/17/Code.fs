module AdventOfCode._2024._17

open System

type Computer =
    { RegisterA: int64
      RegisterB: int64
      RegisterC: int64
      Output: int list }

module Instruction =
    let combo computer operand =
        match operand with
        | 0L | 1L | 2L | 3L -> operand
        | 4L -> computer.RegisterA
        | 5L -> computer.RegisterB
        | 6L -> computer.RegisterC
        | 7L | _ -> failwith "Invalid operand"

    let adv (computer: Computer) (operand: int64) =
        let operand = operand |> combo computer
        let res = computer.RegisterA >>> (int operand)
        { computer with RegisterA = res }, None

    let bxl (computer: Computer) (operand: int64) =
        { computer with RegisterB = computer.RegisterB ^^^ operand },
        None

    let bst (computer: Computer) (operand: int64) =
        let operand = operand |> combo computer
        let struct (_, rem) = Math.DivRem(operand, 8)
        { computer with RegisterB = rem }, None

    let jnz (computer: Computer) (operand: int64) =
        match computer.RegisterA with
        | 0L -> computer, None
        | _ -> computer, Some operand

    let bxc (computer: Computer) (_operand: int64) =
        { computer with RegisterB = computer.RegisterB ^^^ computer.RegisterC },
        None

    let out (computer: Computer) (operand: int64) =
        let operand = operand |> combo computer
        let rem = operand % 8L
        { computer with Output = computer.Output @ [ int rem ] }, None

    let bdv (computer: Computer) (operand: int64) =
        let operand = operand |> combo computer
        let res = computer.RegisterA >>> (int operand)
        { computer with RegisterB = res }, None

    let cdv (computer: Computer) (operand: int64) =
        let operand = operand |> combo computer
        let res = computer.RegisterA >>> (int operand)
        { computer with RegisterC = res }, None

    let fromOpcode opcode =
        match opcode with
        | 0 -> adv
        | 1 -> bxl
        | 2 -> bst
        | 3 -> jnz
        | 4 -> bxc
        | 5 -> out
        | 6 -> bdv
        | 7 -> cdv
        | _ -> failwith "Incorrect opcode"

let getInput () = // Fill by hand
    { RegisterA = 0
      RegisterB = 0
      RegisterC = 0
      Output = List.empty },
    [ 0; 0; 0 ]


let partOne () =
    let mutable instructionPointer = 0
    let mutable computer, instructions = getInput()

    while instructionPointer < instructions.Length do
        let opcode = instructions |> List.item instructionPointer
        let operand = instructions |> List.item (instructionPointer+1)

        let inst = opcode |> Instruction.fromOpcode
        let newcomputer, instDelta = inst computer operand

        computer <- newcomputer
        instructionPointer <-
            match instDelta with
            | Some pointer -> int pointer
            | None -> instructionPointer+2

    computer.Output
    |> List.map string
    |> String.concat ","

// Naive way
// let partTwo () =
    // let tryFind start delta =
    //     let mutable regA: int64 = start
    //     let mutable foundRegAValue = None

    //     while foundRegAValue.IsNone (*&& regA < 100_000_000*) do
    //         // if regA % 100_000L = 0 then
    //         //     printfn "%A: %A" start regA
    //         let mutable instructionPointer = 0
    //         let baseComputer, instructions = getInput()

    //         let mutable computer = { baseComputer with RegisterA = regA }

    //         while instructionPointer < instructions.Length do
    //             let opcode = instructions |> List.item instructionPointer
    //             let operand = instructions |> List.item (instructionPointer+1)

    //             let inst = opcode |> Instruction.fromOpcode
    //             let newComputer, instDelta = inst computer operand

    //             computer <- newComputer
    //             instructionPointer <-
    //                 match instDelta with
    //                 | Some pointer -> int pointer
    //                 | None -> instructionPointer+2

    //         match computer.Output = instructions with
    //         | false -> regA <- regA + delta
    //         | true -> foundRegAValue <- Some regA

    //     foundRegAValue.Value

    // [ (Int32.MaxValue |> int64 |> (+) 0L), 7
    //   (Int32.MaxValue |> int64 |> (+) 1L), 7
    //   (Int32.MaxValue |> int64 |> (+) 2L), 7
    //   (Int32.MaxValue |> int64 |> (+) 3L), 7
    //   (Int32.MaxValue |> int64 |> (+) 4L), 7
    //   (Int32.MaxValue |> int64 |> (+) 5L), 7
    //   (Int32.MaxValue |> int64 |> (+) 6L), 7 ]
    // |> List.mapi (fun i (x, y) ->
    //     printfn "Starting %i" i
    //     Task.Run (fun _ -> tryFind x y)
    // )
    // |> Task.WhenAny
    // |> fun task -> task.Result.Result |> printfn "%i"



let partTwo () =
    let applyLoopIteration (a: int64) =
        let b = a % 8L
        let b = b ^^^ 3
        let c = a >>> int b
        let b = b ^^^ c
        let b = b ^^^ 5
        b % 8L

    let rec findValidAValue i (prog: int list) (answer: int64) =
        match i < prog.Length with
        | false -> Some answer
        | true ->
            let expectedValue = prog[prog.Length-1-i]

            [0..7]
            |> List.choose (fun last3Bits ->
                let a = answer <<< 3 ||| last3Bits
                let output = a |> applyLoopIteration

                match output = expectedValue with
                | false -> None
                | true -> findValidAValue (i+1) prog a
            )
            |> List.tryHead

    let p = getInput() |> snd

    findValidAValue 0 p 0L
    |> printfn "%A"