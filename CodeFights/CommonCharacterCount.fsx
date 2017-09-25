#load "Imports.fsx"
open Swensen.Unquote

let rec retainCommon acc one other =
    match one, other with
    | _, [] -> acc
    | [], _ -> acc
    | h1 :: t1, h2 :: t2 when h1 = h2 -> retainCommon (h1 :: acc) t1 t2
    | h1 :: t1, h2 :: t2 when h1 < h2 -> retainCommon acc t1 (h2::t2)
    | h1 :: t1, _ :: t2 -> retainCommon acc (h1::t1) t2

let commonCharacterCount s1 s2 =
    let sortedList = Seq.sort >> Seq.toList
    retainCommon [] (s1 |> sortedList) (s2 |> sortedList)
    |> Seq.length

printf "testing.."
test <@ commonCharacterCount "abc" "abc" = 3 @>
test <@ commonCharacterCount "aabcc" "adcaa" = 3 @>
printfn "..done!"
