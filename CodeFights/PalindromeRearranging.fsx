#load "Imports.fsx"

open Swensen.Unquote

let palindromeRearranging inputString =
    let isEven n = n % 2 = 0
    let allEvenNumberOfOccurences =
         Seq.forall (fun (_, occurrences) -> occurrences |> isEven)
    let groupedLetters = 
        inputString 
        |> Seq.groupBy id 
        |> Seq.map (fun (letter, occurences) -> letter, occurences |> Seq.length)
    if inputString |> Seq.length |> isEven then
        groupedLetters |> allEvenNumberOfOccurences
    else
        let lettersWithOddOccurrences = 
            groupedLetters 
            |> Seq.where (fun (_,nb) -> not <| isEven nb)
            |> Seq.toList
        match lettersWithOddOccurrences with
        | [letter] -> groupedLetters |> Seq.except [letter] |> allEvenNumberOfOccurences
        | _ -> false

printf "Testing..."
test <@ palindromeRearranging "abcba" @>
test <@ palindromeRearranging "aabbc" @>
test <@ palindromeRearranging "aab" @>
printfn "..Done!"
