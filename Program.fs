module Program

[<EntryPoint>]
let main args =
    match Lexer.lexAll (String.concat " " args) 0 with
    | Ok tokens ->
        printfn "%A" tokens
        0

    | Error msg ->
        printfn "Error: %s" msg
        1
