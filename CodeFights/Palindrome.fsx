#load "Imports.fsx"
open Swensen.Unquote

let reverse s =
        s 
        |> Seq.rev 
        |> Seq.map string 
        |> String.concat ""

let checkPalindrome inputString = 
    inputString = reverse inputString

test <@ not <| checkPalindrome "nope" @>
test <@ checkPalindrome "poop" @>