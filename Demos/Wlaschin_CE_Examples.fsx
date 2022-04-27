open System

let strToInt str =
    try
        str |> int |> Some
    with :? FormatException ->
        None
        
type MyWorkflowBuilder() =
    member _.Bind(x, f) = Option.bind f x
    member _.Return(x) = Some x


let myWorkflow = MyWorkflowBuilder()

let stringAddWorkflow x y z =
    myWorkflow
        {
            let! a = strToInt x
            let! b = strToInt y
            let! c = strToInt z
            return a + b + c
        }
        
// test
let good = stringAddWorkflow "12" "3" "2"
let bad = stringAddWorkflow "12" "xyz" "2"

let strAdd str i =
    match strToInt str with
        | Some n ->
            Some (n + i)
        | None ->
            None
    
let (>>=) m f = Option.bind f m

let good' = strToInt "1" >>= strAdd "2" >>= strAdd "3"            
let bad' = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"            