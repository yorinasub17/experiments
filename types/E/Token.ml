type token =
    | Ident of string
    | Number of float
    | String of string
    | Kwd of char
    | LET
    | BE
    | IN_
