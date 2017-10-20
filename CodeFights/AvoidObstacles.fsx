#load "Imports.fsx"

open Swensen.Unquote

let canJump obstacles distance = 
    if obstacles |> Seq.forall (fun obstacle -> obstacle % distance <> 0) 
    then Some distance
    else None

let avoidObstacles inputArray =
    let sorted = inputArray |> Seq.sort
    let maxJumpDistance = 1 + (sorted |> Seq.max)
    [1..maxJumpDistance]
    |> Seq.choose (canJump sorted)
    |> Seq.head

printf "Testing..."
test <@ avoidObstacles [|5; 3; 6; 7; 9|] = 4 @>
test <@ avoidObstacles [|2;3|] = 4 @>
printfn "..Done!"