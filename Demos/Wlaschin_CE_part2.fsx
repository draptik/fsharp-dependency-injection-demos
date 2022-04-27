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



// Using Combine for sequence generation

type ListBuilder() =
    member this.Bind(m, f) =
        m |> List.collect f
        
    member this.Zero() =
        printfn "Zero"
        []
        
    member this.Yield(x) =
        printfn $"Yield an unwrapped %A{x} as a list"
        [x]
        
    member this.YieldFrom(m) =
        printfn $"Yield a list (%A{m}) directly"
        m
    
    member this.For(m, f) =
        printfn $"For %A{m}"
        this.Bind(m, f)
        
    member this.Combine(a, b) =
        printfn $"Combining %A{a} and %A{b}"
        List.concat [a;b]
        
    member this.Delay(f) =
        printfn "Delay"
        f()
        
let listBuilder = ListBuilder()        

listBuilder {
    yield 1
    yield 2
} |> printfn "Result for yield then yield: %A"

listBuilder {
    yield 1
    yield! [2;3]
} |> printfn "Result for yield then yield: %A"

listBuilder {
    for i in ["red"; "blue"] do
        yield i
        for j in ["hat"; "tie"] do
            yield! [i + " " + j; "-"]
} |> printfn "Result for yield then yield: %A"

// Order of processing for `Combine`


listBuilder {
    yield 1
    yield 2
    yield 3
    yield 4
} |> printfn "Result for yield four times: %A"

// Combine for non-sequences

trace {
    if true then printfn "hello"
    return 1
} |> printfn "Result for combine: %A"

// Implementing combine for workflows with "success" and "failure"

module WorkflowSuccessOrFailure =
    
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
            printfn $"Combining %A{a} and  %A{b}"
            match a with
            | Some _ -> a
            | None -> b
        
        member this.Delay(f) =
            printfn "Delay"
            f()
            
    let trace = TraceBuilder()
    
    // Example: Parsing
    type IntOrBool = I of int | B of bool
    
    let parseInt (s:string) =
        match System.Int32.TryParse(s) with
        | true, i -> Some (I i)
        | false, _ -> None
        
    let parseBool (s:string) =
        match System.Boolean.TryParse(s) with
        | true, b -> Some (B b)
        | false, _ -> None

    trace {
        return! parseBool "42"
        return! parseInt "42"
    } |> printfn "Result for parsing: %A"
    
    // Example: Dictionary lookup
    let map1 = [("1", "One"); ("2", "Two")] |> Map.ofList
    let map2 = [("A", "Alice"); ("B", "Bob")] |> Map.ofList
    
    trace {
        return! map1.TryFind "A"
        return! map2.TryFind "A"
    } |> printfn "Result for map lookup: %A"
    
// Implementing combine for workflows with sequential steps
module WorkflowWithSequentialSteps =
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
            this.Return () // unit not None
        
        member this.Yield(x) =
            printfn $"Yielding an unwrapped %A{x} as an option"
            Some x
            
        member this.YieldFrom(m) =
            printfn $"Yielding an option (%A{m}) directly"
            m
            
        member this.Combine(a, b) =
            printfn $"Combining %A{a} and  %A{b}"
            this.Bind(a, fun () -> b)
        
        member this.Delay(f) =
            printfn "Delay"
            f()
            
    let trace = TraceBuilder()
    
    trace {
        if true then printfn "hello......"
        if false then printfn "......world"
        return 1
    } |> printfn "Result for sequential combine: %A"
    
    