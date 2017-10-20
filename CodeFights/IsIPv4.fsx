#load "Imports.fsx"

open Swensen.Unquote

let split separator (s : string) = s.Split([|separator|])
let tryParse n = 
    match System.Int32.TryParse(n) with
    | true, p -> Some p
    | _ -> None
let inRange =
    function
    | Some n -> 0 <= n && n <= 255
    | None -> false

let isIPv4Address inputString =     
    let numbers = 
        inputString
        |> split '.' 
        |> Seq.map tryParse
    
    (numbers |> Seq.length = 4) && (numbers |> Seq.forall inRange)

printf "Testing..."

test <@ isIPv4Address "172.16.254.1" @>
test <@ not <| isIPv4Address "172.316.254.1" @>
test <@ not <| isIPv4Address ".254.255.0" @>
test <@ isIPv4Address "0.0.0.0" @>
test <@ isIPv4Address "255.255.255.255" @>
test <@ not <| isIPv4Address "256.255.255.255" @>
test <@ not <| isIPv4Address "3" @>
test <@ not <| isIPv4Address "1.2.3.4.5" @>
test <@ not <| isIPv4Address "1.2.3.not a number.4" @>

printfn "..Done!"