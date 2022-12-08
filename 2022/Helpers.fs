[<AutoOpen>]
module Helpers

open System

type String =
    static member fromChars (chars: #seq<char>) =
        chars |> Seq.toArray |> String

    static member splitc (separator: char) (str: string) = str.Split separator
    static member split (separator: string) (str: string) = str.Split separator

type List =
    /// Returns a list that contains all elements of the original list while the given predicate returns True and the first element where the predicate returns False, and then returns no further elements.
    static member takeWhileInclusive (predicate: 'a -> bool) (list: 'a list) =
        list
        |> List.fold
            (fun (list, finished) element ->
                if not finished && element |> predicate then
                    element::list, false
                elif not finished && element |> predicate |> not then
                    element::list, true
                else list, true
            )
            ([], false)
        |> fst
        |> List.rev