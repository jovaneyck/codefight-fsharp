#load "Imports.fsx"
open Swensen.Unquote

let areSimilar a b = 
    let diffs =
        Seq.zip a b
        |> Seq.filter (fun (a,b) -> a <> b)
        |> Seq.toList
    
    match diffs with
    | [] -> true
    | [(a,b); (c,d)] -> a = d && b = c
    | _ -> false


printf "Testing..."

test <@ areSimilar [||] [||] @>
test <@ areSimilar [|1;2;3|] [|1;2;3|] @>
test <@ areSimilar [|1;3;2|] [|1;2;3|] @>
test <@ not <| areSimilar [|3;1;2|] [|1;2;3|] @>

printfn "..done"