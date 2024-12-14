[<AutoOpen>]
module Helpers

open System

module String =
    let split (separator: string) (input: string) =
        input.Split(separator, StringSplitOptions.RemoveEmptyEntries)