# Dependency Injection in F#

Following along Scott Wlaschin's [Dependency Injection in F#](https://fsharpforfunandprofit.com/posts/dependencies/) blog series.

## Content

- [`WlaschinDemo.fs`](Demos/WlaschinDemos.fs): Follow-along code demos
- [`Wlaschin2.fsx`](Demos/Wlaschin2.fsx): The original `fsx` file as posted here: https://gist.github.com/swlaschin/4ed2e4e8ea5b63c968bc469fbce620b5

## Notes

Trying to learn using F# Interactive (FSI) while following the blog series.

Currently using `Console.Readline` doesn't seem to work as expected in FSI.

## FSI: Accessing `modules` in FSI:

If we have a `module` in FSI...

```fsharp
module foo =
    let sayHello =
        printfn "Hello"    
```

...we can access it using `open`.

```fsharp
// within FSI:
open foo
sayHello
```

