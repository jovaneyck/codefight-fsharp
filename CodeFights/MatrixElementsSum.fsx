#load "Imports.fsx"
open Swensen.Unquote

type Room =
    | Haunted
    | Free of int

let onlyValidRooms rooms = 
    rooms
    |> Seq.filter (function Haunted -> false | Free _ -> true)
    |> Seq.map (function Haunted -> 0 | Free r -> r)

let parseColumn (matrix : int[,]) column =
    let elements = matrix.[*,column]
    elements 
    |> Seq.fold 
        (fun (haunted, acc) room -> 
            if room = 0 then true, Haunted :: acc
            elif haunted then true, Haunted :: acc
            else false, Free room :: acc)
        (false, [])
    |> snd

let parse matrix =
    let nbColumns = Array2D.length2 matrix
    [0..(nbColumns-1)]
    |> Seq.map (parseColumn matrix)

let matrixElementsSum matrix = 
    let m = array2D matrix
    parse m
    |> Seq.collect id //flatten into single list
    |> onlyValidRooms
    |> Seq.sum

let matrix = 
    [|
        [|1; 1; 1; 0|]
        [|0; 5; 0; 1|]
        [|2; 1; 3; 10|]
    |]

test <@ matrixElementsSum matrix = 9 @>