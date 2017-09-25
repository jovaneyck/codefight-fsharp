#load "Imports.fsx"
open Swensen.Unquote

type State = {common : char list; other : char list}

let rec stripOne acc letter list = 
    match list with 
    | [] -> acc
    | h :: t when letter = h -> List.append acc t
    | h :: t -> stripOne (h :: acc) letter t

let commonCharacterCount s1 s2 =
    let folder state letter = 
        if state.other |> Seq.contains letter then
            {state with common = letter :: state.common; other = state.other |> stripOne [] letter}
        else
            state
    s1
    |> Seq.fold folder {common = []; other = s2 |> Seq.toList }
    |> fun state -> state.common |> List.length

printf "testing.."
test <@ commonCharacterCount "abc" "abc" = 3 @>
printfn "..done!"
