﻿module Demos

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
        member this.ReadLn() = Console.ReadLine()
        member this.WriteLn str = printfn $"%s{str}"
}

let logger = {
    new ILogger with
        member this.Debug(s) = printfn $"%s{s}"
        member this.Info(s) = printfn $"%s{s}"
        member this.Error(s) = printfn $"%s{s}"
}
    
// Version 1: inject logger as 1st parameter
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

// Version 2: inject logger as last parameter
let compareTwoStrings' str1 str2 (logger:ILogger) =
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

// V1    
let result = compareTwoStrings logger "b" "b"
// V2
let result' = compareTwoStrings' "b" "b" logger

// Version 3: return a function (!)
let compareTwoStrings'' str1 str2 =
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

// V3
//let resultFcn : (ILogger -> ComparisonResult) = compareTwoStrings'' "b" "b"
//let result''a = resultFcn logger
let result''b = (compareTwoStrings'' "b" "b") logger

// V4 Introducing the Reader Monad
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

let compareTwoStrings''' str1 str2 =
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

let readerTmp = compareTwoStrings''' "a" "b"

//Reader.run logger readerTmp

