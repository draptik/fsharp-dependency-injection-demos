// https://fsharpforfunandprofit.com/posts/computation-expressions-builder-part2/

type TraceBuilder() =
    member this.Bind(m, f) =
        match m with
        | None ->
            printfn "Binding with None. Exiting."
        | Some a ->
            printfn $"Binding with Some(%A{a}). Continuing."
        Option.bind f m
        
    member this.Return(x) =
        printfn $"Returning a unwrapped %A{x} as an option."
        Some x
        
    member this.ReturnFrom(m) =
        printfn $"Returning an option (%A{m}) directly"
        m
        
    member this.Zero() =
        printfn "Zero"
        None
    
    member this.Yield(x) =
        printfn $"Yielding an unwrapped %A{x} as an option"
        Some x
        
    member this.YieldFrom(m) =
        printfn $"Yielding an option (%A{m}) directly"
        m
        
    member this.Combine(a, b) =
        match a,b with
        | Some a', Some b' ->
            printfn $"Combining %A{a'} and  %A{b'}"
            Some (a' + b')
        | Some a', None ->
            printfn $"Combining %A{a'} with None"
            Some a'
        | None, Some b' ->
            printfn $"Combining None with %A{b'}"
            Some b'
        | None, None ->
            printfn "Combining None with None"
            None
    
    member this.Delay(f) =
        printfn "Delay"
        f()
        
let trace = TraceBuilder()

// Introducing `Combine`

trace {
    yield 1
    yield 2
} |> printfn "Result for yield then yield: %A"

trace {
    yield 1
    let! x = None
    yield 2
} |> printfn "Result for yield then yield: %A"

trace {
    yield 1
    yield 2
    yield 3
} |> printfn "Result for yield then yield: %A"

trace {
    yield 1
    return 2
} |> printfn "Result for yield then yield: %A"

trace {
    return 1
    return 2
} |> printfn "Result for yield then yield: %A"

    