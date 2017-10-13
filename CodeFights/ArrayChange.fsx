#load "Imports.fsx"
open Swensen.Unquote

type State = {previousValue : int; totalIncrement : int}

let arrayChange a = 
    let folder state element = 
        let diff =
            if element > state.previousValue then 0
            else 1 + (state.previousValue - element)
        {previousValue = element + diff; totalIncrement = state.totalIncrement + diff}

    a 
    |> List.ofArray 
    |> List.fold folder {previousValue = System.Int32.MinValue; totalIncrement = 0}
    |> (fun state -> state.totalIncrement)

printf "Testing..."

test <@ arrayChange [|1;1;1|] = 3 @>

printfn "..done"
