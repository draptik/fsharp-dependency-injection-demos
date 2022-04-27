// https://fsharpforfunandprofit.com/posts/computation-expressions-builder-part1/
open System

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
        
    member _.Zero() =
        printfn "Zero"
        None
        
    member _.Yield(x) =
        printfn $"Yield an unwrapped %A{x} as an option"
        Some x
    
    member _.YieldFrom(m) =
        printfn $"Yield an option (%A{m}) directly"
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

// Introducing `do!`
trace {
    do! Some (printfn "...expression that returns unit")
    do! Some (printfn "...another expression that returns unit")
    let! x = Some (1)
    return x
} |> printfn "Result from do: %A"

// Introducing `Zero`

// Doesn't compile:
//trace {
//} |> printfn "Result for empty: %A"

// Doesn't compile unless Zero is implemented:
trace {
    printfn "hello world"
} |> printfn "Result for empty: %A"

// Introducing `Yield`

// Doesn't compile unless Yield is implemented:
trace {
    yield 1
} |> printfn "Result for yield: %A"

// Doesn't compile unless YieldFrom is implemented:
trace {
    yield! Some 1
} |> printfn "Result for yield!: %A"

// Revisiting `For`
//
type ListBuilder() =
    member this.Bind(m, f) =
        m |> List.collect f
        
    member this.Zero() =
        printfn "Zero"
        []
    
    member this.Return(x) =
        printfn $"Return a unwrapped %A{x} as a list"
        [x]
    
    member this.For(m, f) =
        printfn $"For %A{m}"
        this.Bind(m, f)
        
    member this.Yield(x) =
        printfn $"Yield an unwrapped %A{x} as a list"
        [x]
  

let listBuilder = ListBuilder()

listBuilder {
    let! x = [1..3]
    let! y = [10;20;30]
    return x + y
} |> printfn "Result: %A"

listBuilder {
    for x in [1..3] do
    for y in [10;20;30] do
    return x + y
} |> printfn "Result: %A"