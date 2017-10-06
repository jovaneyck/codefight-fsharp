#load "Imports.fsx"
open Swensen.Unquote

let alternatingSums a =
    let sums = 
        a
        |> Seq.indexed
        |> Seq.groupBy (fun (idx,_) -> idx % 2 = 0)
        |> Seq.map (snd >> (fun g -> g |> Seq.sumBy snd))
        
    if sums |> Seq.length = 1 then
        Seq.append sums [0]
    else
        sums
    |> Seq.toArray

printf "testing.."

test <@ alternatingSums [50; 60; 60; 45; 70] = [|180; 105|] @>

printf "..done"
