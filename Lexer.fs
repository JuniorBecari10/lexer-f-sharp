module Lexer

open System
open Computation

type Token =
    | Number of float
    | Plus
    | Minus
    | Star
    | Slash
    | LParen
    | RParen
    | Eof

let isDigit c = Char.IsDigit c
let isSpace c = Char.IsWhiteSpace c
let isNumber c = isDigit c || c = '.'

let (>>=) res variant = res |> Result.map (fun ok -> variant :: ok)

type LexerResult = Result<Token list, string>

let rec lexAll (input: string) (pos: int): LexerResult =
    if pos >= input.Length then
        Ok [Eof]
    else
        let c = input.[pos]
        let next () = lexAll input (pos + 1)

        match c with
        | _ when isSpace c -> next ()

        | '+' -> next () >>= Plus
        | '-' -> next () >>= Minus
        | '*' -> next () >>= Star
        | '/' -> next () >>= Slash
        | '(' -> next () >>= LParen
        | ')' -> next () >>= RParen

        | _ when isNumber c ->
            result {
                let sb = Text.StringBuilder ()
                let rec readNum i =
                    if i < input.Length && isNumber input.[i] then
                        sb.Append(input.[i]) |> ignore
                        readNum (i + 1)
                    else i

                let nextDigit = readNum pos

                let! value =
                    match Double.TryParse (sb.ToString ()) with
                        | true, value -> Ok value
                        | _ -> Error "Cannot parse number."

                let! res = lexAll input nextDigit
                return! Ok (Number value :: res)
            }

        | _ -> Error (sprintf "Unknown character: '%c'" c)
