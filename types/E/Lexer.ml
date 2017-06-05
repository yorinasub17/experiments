let rec lex = parser
    (* skip any whitespace *)
    | [< ' (' ' | '\n' | '\t'); stream >] -> lex stream
    (* identifiers are a stream of contiguous characters a-zA-Z0-9 *)
    | [< ' ('A' .. 'Z' | 'a' .. 'z' as c); stream >] ->
            let buffer = Buffer.create 1 in
            Buffer.add_char buffer c;
            lex_ident buffer stream
    (* number literal *)
    | [< ' ('0' .. '9' as c); stream >] ->
            let buffer = Buffer.create 1 in
            Buffer.add_char buffer c;
            lex_number buffer stream
    (* string literal *)
    | [< ' ('"'); stream >] ->
            let buffer = Buffer.create 1 in
            lex_string buffer stream
    (* comments *)
    | [< ' ('#'); stream >] ->
            lex_comment stream
    (* any single unrecognized char is a kwd *)
    | [< ' c; stream >] ->
            [< ' Token.Kwd c; lex stream >]
    (* end of stream *)
    | [< >] -> [< >]

(* rules for resolving an identifier, which may be a reserved word *)
and lex_ident buffer = parser
    (* Keep consuming a-zA-Z0-9 while there is no space *)
    | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as c); stream >] ->
            Buffer.add_char buffer c;
            lex_ident buffer stream
    (* if something else, then stop consuming and resolve it *)
    | [< stream=lex >] ->
            match Buffer.contents buffer with
            (* the three reserved words *)
            | "let" -> [< ' Token.LET; stream >]
            | "be" -> [< ' Token.BE; stream >]
            | "in" -> [< ' Token.IN_; stream >]
            (* the rest is rolled into an identifier *)
            | id -> [< ' Token.Ident id; stream >]

(* rules for resolving a contiguous list of numbers *)
and lex_number buffer = parser
    | [< ' ('0' .. '9' | '.' as c); stream >] ->
            Buffer.add_char buffer c;
            lex_number buffer stream
    | [< stream=lex >] ->
            [< 'Token.Number (float_of_string (Buffer.contents buffer)); stream >]

(* rules for resolving a string literal *)
and lex_string buffer = parser
    | [< ' ('"'); stream=lex >] ->
            [< ' Token.String (Buffer.contents buffer); stream >]
    (* include escaped quotes *)
    | [< ' ('\\'); ' ('"'); stream>] ->
            Buffer.add_char buffer '"';
            lex_string buffer stream
    | [< 'c; stream>] ->
            Buffer.add_char buffer c;
            lex_string buffer stream

(* rules for resolving comments, by stripping them out *)
and lex_comment = parser
    | [< ' ('\n'); stream=lex >] -> stream
    | [< 'c; e=lex_comment >] -> e
    | [< >] -> [< >]
