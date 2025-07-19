module Lexer

open System
open Computation
open Token

let isDigit c = Char.IsDigit c
let isSpace c = Char.IsWhiteSpace c
let isDigitOrDot c = isDigit c || c = '.'

let (>>=) res variant = res |> Result.map (fun ok -> variant :: ok)

type LexerError = {
    message: string
    pos: int
}

type LexerResult = Result<Token list, LexerError>

let rec lexAll (input: string) (pos: int): LexerResult =
    let token kind = {
        kind = kind
        pos = pos
    }

    let error msg = {
        message = msg
        pos = pos
    }

    if pos >= input.Length then
        Ok [token Eof]
    else
        let c = input.[pos]
        let next () = lexAll input (pos + 1)

        match c with
        | _ when isSpace c -> next ()

        | '+' -> next () >>= token Plus
        | '-' -> next () >>= token Minus
        | '*' -> next () >>= token Star
        | '/' -> next () >>= token Slash
        | '(' -> next () >>= token LParen
        | ')' -> next () >>= token RParen

        | _ when isDigitOrDot c -> lexNumber input pos token error
        | _ -> Error (error (sprintf "Unknown character: '%c'" c))

and lexNumber input pos token error =
    result {
        let sb = Text.StringBuilder ()
        let rec readNum i =
            if i < input.Length && isDigitOrDot input.[i] then
                sb.Append(input.[i]) |> ignore
                readNum (i + 1)
            else i

        let nextDigit = readNum pos

        // fix
        let! value =
            match Double.TryParse (sb.ToString ()) with
                | true, value -> Ok value
                | _ -> Error (error "Cannot parse number.")

        let! res = lexAll input nextDigit
        return! Ok (token (Number value) :: res)
    }
