#load "Imports.fsx"
open Swensen.Unquote

let adjacentElementsProduct inputArray =
    inputArray
    |> Seq.pairwise
    |> Seq.map (fun (a,b) -> a * b)
    |> Seq.max

test <@ adjacentElementsProduct [|0;0|] = 0 @>
test <@ adjacentElementsProduct [|2;3|] = 6 @>
test <@ adjacentElementsProduct [|1;2;3|] = 6 @>
test <@ adjacentElementsProduct [|1;2;3;1|] = 6 @>