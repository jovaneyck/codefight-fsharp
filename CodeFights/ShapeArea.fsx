#load "Imports.fsx"
open Swensen.Unquote

let rec shapeArea n =
    if n = 1 
    then 1
    else
        let previous = shapeArea (n-1)
        let border = 4 * (n-1)
        previous + border

[
    1,1
    2,5
    3,13
    4,25
    5,41
]
|> List.iter (fun (n, expected) -> test <@ shapeArea n = expected @>)