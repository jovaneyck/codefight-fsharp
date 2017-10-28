#load "Imports.fsx"
open Swensen.Unquote

let relevantIndices nbRows nbColumns =
    [for r in 1 .. nbRows - 2 do
     for c in 1 .. nbColumns - 2 -> (r,c)]
 
let values matrix =
    let nbRows = (matrix |> Array2D.length1) - 1
    let nbColumns = (matrix |> Array2D.length2) - 1
    [for r in 0 .. nbRows do
     for c in 0 .. nbColumns -> matrix.[r,c]]

let averageValue values =
    values
    |> Seq.map double
    |> Seq.average
    |> int

let averageFor (bitmap : int [,]) (row, column) =
    let neighbourhood = bitmap.[row-1..row+1,column-1..column+1]
    neighbourhood |> values |> averageValue

let avg bitmap indices = 
    bitmap
    |> Array2D.mapi (fun r c _ -> 
        if indices |> Seq.contains (r,c) 
        then Some <| averageFor bitmap (r,c) 
        else None)
        
let trim (m : int option [,]) =
    [| for r in [1..(m |> Array2D.length1) - 2] ->
        m.[r, 1..(m |> Array2D.length2) - 2]
    |]

let toValues = Array.choose id

let boxBlur image : int array array =
    let bitmap = array2D image
    let nbRows = bitmap |> Array2D.length1
    let nbColumns = bitmap |> Array2D.length2
    relevantIndices nbRows nbColumns
    |> avg bitmap
    |> trim
    |> Array.map toValues

printf "Testing..."
test <@ boxBlur [|
                    [|1; 1; 1|] 
                    [|1; 7; 1|]
                    [|1; 1; 1|]
                |] =
                    [|[|1|]|] @>
test <@ boxBlur [|
                    [|7; 4; 0; 1|]
                    [|5; 6; 2; 2|] 
                    [|6; 10; 7; 8|] 
                    [|1; 4; 2; 0|]
                |] =
                [|
                    [|5;4|]
                    [|4;4|]
                |] @>
printfn "..Done!"