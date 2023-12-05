open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let input = File.ReadAllLines "./input.txt"
// let input = File.ReadAllLines "./2023/04/test-input.txt"

type ScratchedCard =
    { CardId: int
      WinningNumbers: int array }

    static member getPoints card =
        match card.WinningNumbers.Length with
        | 0 -> 0
        | x -> 2. ** float (x - 1) |> int

type Card =
    { CardId: int
      WinningNumbers: int array
      NumbersToScratch: int array }

    static member scratch (card: Card) =
        { CardId = card.CardId
          WinningNumbers =
            Set.intersect
                (card.WinningNumbers |> Set.ofArray)
                (card.NumbersToScratch |> Set.ofArray)
            |> Set.toArray }

    static member parse (str: string) =
        let regex = Regex "Card\s+(\d+): (.+) \| (.+)"
        let m = regex.Match str

        { CardId = m.Groups[1].Value |> int
          WinningNumbers = m.Groups[2].Value.Split " " |> Array.filter ((<>) "") |> Array.map int
          NumbersToScratch = m.Groups[3].Value.Split " " |> Array.filter ((<>) "") |> Array.map int }

    // static member getWinnedCards

let part1 =
    input
    |> Array.map (Card.parse >> Card.scratch >> ScratchedCard.getPoints)
    |> Array.sum

// --- Part 2 ---
let part2 =
    let winnedCardsStore = List<int>()
    let cards = input |> Array.map Card.parse

    for card in cards do
        let winnedCardsNumber = card |> Card.scratch |> _.WinningNumbers.Length

        let winnedCardsToScratch =
            winnedCardsStore
            |> Array.ofSeq
            |> Array.filter ((=) card.CardId)
            |> Array.append [| card.CardId |]

        // printfn "Scratched card %i %i times" card.CardId (winnedCardsToScratch |> Array.length)

        // Add winned cards to the list
        let winnedCards: Card array =
            Array.init
                winnedCardsNumber
                (fun i -> cards |> Array.item (i + card.CardId))

        winnedCardsToScratch |> Array.iter (fun _cardId ->
            winnedCards |> Array.iter (_.CardId >> winnedCardsStore.Add)
        )

    winnedCardsStore.Count + cards.Length

printfn "Part 1: %A" part1
printfn "Part 2: %A" part2