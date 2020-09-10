// Learn more about F# at http://fsharp.org

open System
open LogisticService.RoutePlan

let validChars c =
    match c with 
        |"A" | "B" -> true
        | _ -> false
        
let validateInput input =
    match Array.length input with
    | 0 -> false
    | _ -> Array.forall validChars input

let run input =
    plan input

[<EntryPoint>]
let main argv =
    
    match argv with
        | a when validateInput argv -> run a |> printfn "%i"
        | _ when not (validateInput argv) -> printfn "The provided input contains errors, please specify the goods list delimited with space"
        | _ -> ()
    0 // return an integer exit code
