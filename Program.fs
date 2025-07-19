module Program
open System

let input prompt =
    printf "%s" prompt
    Console.ReadLine ()

[<EntryPoint>]
let main _ =
    while true do
        let inp = input "> "
        match Lexer.lexAll inp 0 with
        | Ok tokens ->
            printfn "%A" tokens

        | Error err ->
            printfn "%s^ %s" (String.replicate (err.pos + 2) " ") err.message

    0
