// https://fsharpforfunandprofit.com/posts/computation-expressions-builder-part1/
type TraceBuilder() =
    member _.Bind(m, f) =
        match m with
        | None ->
            printfn "Binding with None. Exiting."
        | Some a ->
            printfn $"Binding with Some(%A{a}). Continuing"
        Option.bind f m
        
    member _.Return(x) =
        printfn $"Returning a unwrapped %A{x} as an option"
        Some x
        
    member _.ReturnFrom(m) =
        printfn $"Returning an option (%A{m}) directly"
        m
        
let trace = TraceBuilder()

trace {
    return 1
    } |> printfn "Result 1: %A"

trace {
    return! Some 2
    } |> printfn "Result 2: %A"

trace {
    let! x = Some 1
    let! y = Some 2
    return x + y
    } |> printfn "Result 3: %A"

trace {
    let! x = None
    let! y = Some 1
    return x + y
    } |> printfn "Result 4: %A"