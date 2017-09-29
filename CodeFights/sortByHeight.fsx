#load "Imports.fsx"
open Swensen.Unquote

type Space = Tree | Free | Person of int

let rec addToPark park person =
    match park with
    | Free :: p -> (Person person) :: p
    | other :: p -> other :: (addToPark p person)

let rec toNumbers park =
    match park with
    | [] -> []
    | Tree :: p -> -1 :: (toNumbers p)
    | Person p :: rest -> p :: (toNumbers rest)

let sortByHeight array = 
    let a = array |> List.ofArray
    let parkLayout = a |> List.map (fun n -> if n = -1 then Tree else Free)
    let sortedPeople = a |> List.filter (fun n -> n <> -1) |> List.sort
    let sortedPark = sortedPeople |> List.fold addToPark parkLayout
    sortedPark |> toNumbers |> List.toArray

printf "testing.."
test <@ sortByHeight [|-1; 150; 190; 170; -1; -1; 160; 180|] = [|-1; 150; 160; 170; -1; -1; 180; 190|] @>
printfn "..done!"
