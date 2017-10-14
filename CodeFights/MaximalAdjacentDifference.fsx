#load "Imports.fsx"
open Swensen.Unquote

let arrayMaximalAdjacentDifference inputArray =
    inputArray
    |> List.pairwise
    |> List.maxBy (fun (f,s) -> abs (f - s))

printf "Testing..."

test <@ arrayMaximalAdjacentDifference [2;4;1;0] = 3 @>

printfn "..done"