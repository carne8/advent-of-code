[<AutoOpen>]
module Helpers

open System

type String =
    static member fromChars(chars: #seq<char>) = chars |> Seq.toArray |> String
    static member splitc (separator: char) (str: string) = str.Split separator |> Array.toList
    static member split (separator: string) (str: string) = str.Split separator |> Array.toList
    static member trim (str: string) = str.Trim()
    static member substring startIndex (str: string) = str.Substring startIndex

module List =
    /// Returns a list that contains all elements of the original list while the given predicate returns True and the first element where the predicate returns False, and then returns no further elements.
    let takeWhileInclusive (predicate: 'a -> bool) (list: 'a list) =
        list
        |> List.fold
            (fun (list, finished) element ->
                if not finished && element |> predicate then
                    element :: list, false
                elif not finished && element |> predicate |> not then
                    element :: list, true
                else
                    list, true)
            ([], false)
        |> fst
        |> List.rev

    /// Replace the N element by the passed value.
    let replace index newElement list =
        list
        |> List.mapi (fun i element ->
            if i = index then
                newElement
            else
                element)

    let mapOne index f list =
        list
        |> List.mapi (fun i element ->
            if i = index then
                element |> f
            else
                element)

module Math =
    let abs: int -> int = Math.Abs
    let clamp (x: int) (a, b) = Math.Clamp(x, a, b)
