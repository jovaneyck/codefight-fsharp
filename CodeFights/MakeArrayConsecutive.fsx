#load "Imports.fsx"
open Swensen.Unquote

let nbMissing (one, other) =
    other - one - 1

let makeArrayConsecutive2 statues =
    statues 
    |> Seq.sort
    |> Seq.pairwise
    |> Seq.map nbMissing
    |> Seq.sum

[
    [|6; 2; 3; 8|], 3
]
|> List.iter (fun (statues, expected) -> test <@ makeArrayConsecutive2 statues = expected @>)