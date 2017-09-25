#load "Imports.fsx"
open Swensen.Unquote

let digits number =
    let rec digits acc number =
        if number < 10 then
            number :: acc
        else
            let digit = number % 10
            digits (digit :: acc) (number / 10)
    digits [] number

let isLucky number = 
    let [first; second] =
        number
        |> digits
        |> List.splitInto 2
    (first |> List.sum) = (second |> List.sum)

printf "testing.."
test <@ isLucky 1230 @>
test <@ isLucky 239017 |> not @>
printfn "..done!"
