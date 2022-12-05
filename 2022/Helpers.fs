[<AutoOpen>]
module Helpers

open System

type String =
    static member fromChars (chars: #seq<char>) =
        chars |> Seq.toArray |> String

    static member splitC (separator: char) (str: string) = str.Split separator
    static member split (separator: string) (str: string) = str.Split separator