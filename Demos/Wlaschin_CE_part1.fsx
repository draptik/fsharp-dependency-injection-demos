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

        