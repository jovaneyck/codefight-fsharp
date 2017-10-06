#load "Imports.fsx"
open Swensen.Unquote

let addBorder picture =
    let width = picture |> Array.head |> Seq.length
    let fullWidthFrame = Seq.replicate (width + 2) "*" |> String.concat "" |> Array.singleton
    let innerLines = picture |> Array.map (sprintf "*%s*")
    Array.concat [fullWidthFrame; innerLines; fullWidthFrame]

let picture = [|"abc";"ded"|]
printf "testing.."

test <@ addBorder picture = [| "*****"; "*abc*"; "*ded*"; "*****"|] @>

printf "..done"
