#load "Imports.fsx"
open Swensen.Unquote

let innerParentheses s =
    [for m in System.Text.RegularExpressions.Regex.Matches(s, "\(([a-zA-Z]|\s)*\)") -> m.Value]

let reverseContent (parenthesized : string) =
    parenthesized.Substring(1, parenthesized.Length-2)
    |> Seq.rev
    |> Seq.map string
    |> String.concat ""

let reverseInnerParentheses (s : string) (parentheses : string list) =
    let folder (s : string) (p : string) = s.Replace(p, reverseContent p)
    parentheses |> List.fold folder s

let reverseParentheses s = 
    let rec fixPoint s =
        let next =
            s 
            |> innerParentheses
            |> (reverseInnerParentheses s)
        if next = s then s
        else fixPoint next
    fixPoint s

printf "testing.."

test <@ reverseParentheses "abc" = "abc" @> 
test <@ reverseParentheses "a(b)c" = "abc" @> 
test <@ reverseParentheses "a(bc)d" = "acbd" @> 
test <@ reverseParentheses "a(b(cd))e" = "acdbe" @>
test <@ reverseParentheses "a(b(cd)e)f" = "aecdbf" @>

printf "..done"