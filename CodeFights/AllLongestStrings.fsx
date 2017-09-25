#load "Imports.fsx"
open Swensen.Unquote

let orElse def value = defaultArg value def
let allLongestStrings inputArray =
    inputArray
    |> Array.groupBy Seq.length
    |> Array.sortByDescending fst
    |> Array.tryHead
    |> Option.map snd
    |> orElse [||]

printf "testing.."
test <@ allLongestStrings [||] |> Array.isEmpty @>
test <@ [|"a";"b";"c"|] |> allLongestStrings = [|"a";"b";"c"|] @>
test <@ [|"aa";"b";"c"|] |> allLongestStrings = [|"aa"|] @>
printfn "..done!"