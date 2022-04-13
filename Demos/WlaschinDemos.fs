module Demos

open System
(*
Six approaches to dependency injection

https://fsharpforfunandprofit.com/posts/dependencies/
*)

type ComparisonResult =
    | Bigger
    | Smaller
    | Equal
    
type ILogger =
    abstract Debug : string -> unit
    abstract Info : string -> unit
    abstract Error : string -> unit

type IConsole =
    abstract ReadLn : unit -> string
    abstract WriteLn : string -> unit

let console = {
    new IConsole with
        member _.ReadLn() = Console.ReadLine()
        member _.WriteLn str = printfn $"%s{str}"
}

let logger = {
    new ILogger with
        member _.Debug(s) = printfn $"%s{s}"
        member _.Info(s) = printfn $"%s{s}"
        member _.Error(s) = printfn $"%s{s}"
}

let happyLogger = {
        new ILogger with
            member _.Debug(s) = printfn $"🤔🤔🤔 %s{s}"
            member _.Info(s) = printfn $"😀😀😀 %s{s}"
            member _.Error(s) = printfn $"😡😡😡 %s{s}"
}

type Reader<'env,'a> = Reader of action : ('env -> 'a)

module Reader =
    /// Run a Reader with a given environment
    let run env (Reader action)  =
        action env  // simply call the inner function

    /// Create a Reader which returns the environment itself
    let ask = Reader id

    /// Map a function over a Reader
    let map f reader =
        Reader (fun env -> f (run env reader))

    /// flatMap a function over a Reader
    let bind f reader =
        let newAction env =
            let x = run env reader
            run env (f x)
        Reader newAction

type ReaderBuilder() =
    member _.Return(x) = Reader (fun _ -> x)
    member _.Bind(x,f) = Reader.bind f x
    member _.Zero() = Reader (fun _ -> ())

// the builder instance
let reader = ReaderBuilder()

// Version 1: inject logger as 1st parameter
module V1_LoggingAsFirstParameter =
    let compareTwoStrings (logger:ILogger) str1 str2 =
        logger.Debug "compareTwoStrings: Starting"

        let result =
            if str1 > str2 then
                Bigger
            else if str1 < str2 then
                Smaller
            else
                Equal

        logger.Info $"compareTwoStrings: result=%A{result}"
        logger.Debug "compareTwoStrings: Finished"
        result        

    let result = compareTwoStrings logger "b" "b"

// Version 2: inject logger as last parameter    
module V2_LoggingAsLastParameter =
    let compareTwoStrings str1 str2 (logger:ILogger) =
        logger.Debug "compareTwoStrings: Starting"

        let result =
            if str1 > str2 then
                Bigger
            else if str1 < str2 then
                Smaller
            else
                Equal

        logger.Info $"compareTwoStrings: result=%A{result}"
        logger.Debug "compareTwoStrings: Finished"
        result        

    let result' = compareTwoStrings "b" "b" logger

// Version 3: return a function (!)
module V3_ReturningAFunction =
    let compareTwoStrings str1 str2 =
        fun (logger:ILogger) ->
            logger.Debug "compareTwoStrings: Starting"

            let result =
                if str1 > str2 then
                    Bigger
                else if str1 < str2 then
                    Smaller
                else
                    Equal

            logger.Info $"compareTwoStrings: result=%A{result}"
            logger.Debug "compareTwoStrings: Finished"
            result        

    let resultFcn : (ILogger -> ComparisonResult) = compareTwoStrings "b" "b"
    let resultA = resultFcn logger
    let resultB = (compareTwoStrings "b" "b") logger

// Version 4: Introducing the Reader Monad
module V4_ReaderMonad =
    let compareTwoStrings str1 str2 =
        reader {
            let! (logger:ILogger) = Reader.ask
            logger.Debug "compareTwoStrings: Starting"

            let result =
                if str1 > str2 then
                    Bigger
                else if str1 < str2 then
                    Smaller
                else
                    Equal

            logger.Info $"compareTwoStrings: result=%A{result}"
            logger.Debug "compareTwoStrings: Finished"
            return result
        }

    let readerTmp = compareTwoStrings "a" "b"
    
    (*
    Test:

    Reader.run logger readerTmp
    Reader.run happyLogger readerTmp

    *)    

    // readFromConsole
    let readFromConsole() =
        reader {
            let! (console : IConsole) = Reader.ask
            console.WriteLn "Enter the first value"
            let str1 = console.ReadLn
            console.WriteLn "Enter the second value"
            let str2 = console.ReadLn
            
            return str1, str2
        }


// Version 5:
module V5_ReaderMonad =
    let compareTwoStrings str1 str2 =
        reader {
            let! (logger:#ILogger) = Reader.ask
            logger.Debug $"compareTwoStrings: Starting to compare %A{str1} and %A{str2}"

            let result =
                if str1 > str2 then
                    Bigger
                else if str1 < str2 then
                    Smaller
                else
                    Equal

            logger.Info $"compareTwoStrings: result=%A{result}"
            logger.Debug "compareTwoStrings: Finished"
            return result
        }
    
    let readFromConsole() =
        reader {
            let! (console:#IConsole) = Reader.ask
            let! (logger:#ILogger) = Reader.ask     // OK now!
            console.WriteLn "Enter the first value"
            let str1 = "a" //console.ReadLn()
            console.WriteLn "Enter the second value"
            let str2 = "b" //console.ReadLn()
            logger.Info $"Done reading from console: Found values %A{str1} and %A{str2}"
            
            return str1, str2
        }
        
    let writeToConsole (result:ComparisonResult) =
        reader {
            let! (console:#IConsole) = Reader.ask

            match result with
            | Bigger ->
                console.WriteLn "The first value is bigger"
            | Smaller ->
                console.WriteLn "The first value is smaller"
            | Equal ->
                console.WriteLn "The values are equal"
    }
        
    type IServices =
        inherit ILogger
        inherit IConsole
    
    let program : Reader<IServices,_> =
        reader {
            let! str1, str2 = readFromConsole()
            let! result = compareTwoStrings str1 str2
            do! writeToConsole result
        }
        
    let services =
        {
            new IServices
            interface IConsole with
                member _.ReadLn() = console.ReadLn()
                member _.WriteLn str = console.WriteLn str
            interface ILogger with
                member _.Debug str = logger.Debug str
                member _.Info str = logger.Info str
                member _.Error str = logger.Error str
        }
    
    Reader.run services program 
                    
