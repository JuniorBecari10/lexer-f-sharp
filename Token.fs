module Token

type TokenKind =
    | Number of float
    | Plus
    | Minus
    | Star
    | Slash
    | LParen
    | RParen
    | Eof

type Token = {
    kind: TokenKind
    pos: int
}
