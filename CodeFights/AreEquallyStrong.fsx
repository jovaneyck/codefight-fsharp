#load "Imports.fsx"
open Swensen.Unquote

let areEquallyStrong yourLeft yourRight friendsLeft friendsRight =
    let [youWeakest; youStrongest] = [yourLeft; yourRight]|> List.sort    
    let [friendWeakest; friendStrongest] = [friendsLeft; friendsRight] |> List.sort
    youWeakest = friendWeakest && youStrongest = friendStrongest


printf "Testing..."

test <@ areEquallyStrong 10 15 15 10 @>
test <@ areEquallyStrong 15 10 15 10 @>
test <@ not <| areEquallyStrong 15 10 15 9 @>

printfn "..done"

