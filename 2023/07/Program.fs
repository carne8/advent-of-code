open System.IO

let input = File.ReadAllLines "./input.txt"
let testInput = File.ReadAllLines "./test-input.txt"

type Hand =
    | FiveOfAKind  of string // AAAAA A
    | FourOfAKind  of string // AA8AA A, 8
    | FullHouse    of string // 22333 3, 2
    | ThreeOfAKind of string // TTT98 T, 9, 8
    | TwoPair      of string // 23432 3, 2, 4
    | OnePair      of string // A23A4 A, 432
    | HighCard     of string // 23456

    static member private chars withJoker =
        match withJoker with
        | false -> "AKQJT98765432" |> Seq.toArray
        | true -> "AKQT98765432J" |> Seq.toArray

    static member rawString =
        function
        | FiveOfAKind  str
        | FourOfAKind  str
        | FullHouse    str
        | ThreeOfAKind str
        | TwoPair      str
        | OnePair      str
        | HighCard     str -> str

    static member compareStrings withJoker h1 h2 =
        let strH1 = h1 |> Hand.rawString
        let strH2 = h2 |> Hand.rawString
        match strH1 = strH2 with
        | true -> 0
        | false ->
            let mutable differentChars = None
            let mutable i = 0

            while differentChars |> Option.isNone do
                let char1, char2 = strH1[i], strH2[i]

                match char1 <> char2 with
                | true -> differentChars <- Some (char1, char2)
                | false -> i <- i+1 // And continue iterating

            let char1, char2 = differentChars.Value
            let idx1 = Hand.chars withJoker |> Array.findIndex ((=) char2)
            let idx2 = Hand.chars withJoker |> Array.findIndex ((=) char1)

            idx1 - idx2

    static member compare withJoker h1 h2 =
        (h1 |> Hand.getRank) - (h2 |> Hand.getRank)
        |> function
            | 0 -> Hand.compareStrings withJoker h1 h2
            | x -> x * 10

    static member getRank =
        function
        | FiveOfAKind  _ -> 7
        | FourOfAKind  _ -> 6
        | FullHouse    _ -> 5
        | ThreeOfAKind _ -> 4
        | TwoPair      _ -> 3
        | OnePair      _ -> 2
        | HighCard     _ -> 1

    static member parse (str: string) =
        str
        |> List.ofSeq
        |> List.distinct
        |> List.map (fun char ->
            str
            |> Seq.filter ((=) char)
            |> Seq.length
        )
        |> List.sortDescending
        |> function
            | [ 5 ] ->   FiveOfAKind  str // AAAAA
            | 4::_ ->    FourOfAKind  str // AA8AA
            | 3::2::_ -> FullHouse    str // 22333
            | 3::_ ->    ThreeOfAKind str // TTT98
            | 2::2::_ -> TwoPair      str // 23432
            | 2::_ ->    OnePair      str // A23A4
            | _ ->       HighCard     str // 23456

    static member parseWithJokers joker (str: string) =
        let chars =
            str
            |> List.ofSeq
            |> List.groupBy id
            |> List.map (fun (char, x) -> char, x |> List.length)

        let maxChar = chars |> List.maxBy snd

        match maxChar |> fst = joker with
        | true -> chars |> List.map snd
        | false ->
            let jokers =
                chars
                |> List.tryFind (fst >> (=) joker)
                |> Option.map snd
                |> Option.defaultValue 0

            chars
            |> List.filter (fst >> (<>) joker)
            |> List.map (fun (char, appearedTimes) ->
                match char = fst maxChar with
                | true -> appearedTimes + jokers
                | false -> appearedTimes
            )

        |> List.sortDescending
        |> function
            | [ 5 ] ->   FiveOfAKind str
            | 4::_ ->    FourOfAKind str
            | 3::2::_ -> FullHouse str
            | 3::_ ->    ThreeOfAKind str
            | 2::2::_ -> TwoPair str
            | 2::_ ->    OnePair str
            | _ ->       HighCard str

let part1 : string array -> int =
    Array.map (fun line ->
        match line.Split " " with
        | [| hand; bid |] -> hand |> Hand.parse, int bid
        | other           -> failwithf "Failed to parse line: %A" other
    )
    >> Array.sortWith (fun (h1, _) (h2, _) -> Hand.compare false h1 h2)
    >> Array.mapi (fun index (_, bid) -> (index + 1) * bid)
    >> Array.sum

let part2 : string array -> int =
    Array.map (fun line ->
        match line.Split " " with
        | [| hand; bid |] -> hand |> Hand.parseWithJokers 'J', int bid
        | other           -> failwithf "Failed to parse line: %A" other
    )
    >> Array.sortWith (fun (h1, _) (h2, _) -> Hand.compare true h1 h2)
    >> Array.mapi (fun index (_, bid) -> (index + 1) * bid)
    >> Array.sum

printfn "Part 1: %i" (part1 testInput)
printfn "Part 1: %i" (part1 input)
printfn "Part 2: %i" (part2 testInput)
printfn "Part 2: %i" (part2 input)