// https://fsharpforfunandprofit.com/posts/computation-expressions-builder-part3/

type TraceBuilder() =
    member this.Bind(m, f) =
        match m with
        | None ->
            printfn "Binding with None. Exiting."
        | Some a ->
            printfn $"Binding with Some(%A{a}). Continuing"
        Option.bind f m

    member this.Return(x) =
        printfn $"Return an unwrapped %A{x} as an option"
        Some x

    member this.Zero() =
        printfn "Zero"
        None

    member this.Combine (a,b) =
        printfn $"Returning early with %A{a}. Ignoring second part: %A{b}"
        a

    member this.Delay(funcToDelay) =
        let delayed = fun () ->
            printfn $"%A{funcToDelay} - Starting Delayed Fn."
            let delayedResult = funcToDelay()
            printfn $"%A{funcToDelay} - Finished Delayed Fn. Result is %A{delayedResult}"
            delayedResult  // return the result

        printfn $"%A{funcToDelay} - Delaying using %A{delayed}"
        delayed // return the new function

    
    member this.Run(funcToRun) =
        printfn $"%A{funcToRun} - Run Start."
        let runResult = funcToRun()
        printfn $"%A{funcToRun} - Run End. Result is %A{runResult}"
        runResult // return the result of running the delayed function
        
// make an instance of the workflow
let trace = new TraceBuilder()
//
//trace {
//    printfn "Part 1: about to return 1"
//    return 1
//    printfn "Part 2: after return has happened"
//    } |> printfn "Result for Part1 without Part2: %A"
//
//let f = trace {
//    printfn "Part 1: about to return 1"
//    return 1
//    printfn "Part 2: after return has happened"
//    }
//f() |> printfn "Result for Part1 without Part2: %A"

trace {
    printfn "Part 1: about to return 1"
    return 1
    printfn "Part 2: after return has happened"
    } |> printfn "Result for Part1 without Part2: %A"