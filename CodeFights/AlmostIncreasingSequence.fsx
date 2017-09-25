#load "Imports.fsx"
open Swensen.Unquote

let isOffending pair =
    let ((_,a),(_,b)) = pair
    a >= b

let findOffendingElements list =
    list
    |> List.indexed
    |> List.pairwise
    |> List.filter isOffending

let removeElementAt index array =
    let (before, afterIncluding) = array |> List.splitAt index
    List.append before (List.tail afterIncluding)

let isStrictlyIncreasing sequence = 
    sequence
    |> Seq.pairwise
    |> Seq.forall (fun (a,b) -> a < b)

let buildCandidateSequences list offender =
    let ((aindex,_), (bindex, _)) = offender
    let one = list |> removeElementAt aindex
    let other = list |> removeElementAt bindex
    [|one;other|]

let almostIncreasingSequence sequence =
    let asList = sequence |> List.ofArray
    let offenders = asList |> findOffendingElements
    
    offenders
    |> Seq.collect (buildCandidateSequences asList)
    |> Seq.exists isStrictlyIncreasing

[
    [|1;3;2;1|],    false
    [|1;3;2|],      true
    [|1;1;1;2;3|], false
    [|1; 2; 3; 4; 3; 5|], true
    [|1;2;1;2|], false
    [|0; -2; 5; 6|], true
]
|> List.iter (fun (sequence, expected) -> test <@ almostIncreasingSequence sequence = expected @>)

let huge_test_case = Array.append [|1..(pown 10 4)|] [|100003;100002|]
test <@ almostIncreasingSequence huge_test_case @>