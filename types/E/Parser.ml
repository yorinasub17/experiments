let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10

let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

let rec parse_primary = parser
    (* number literal expr *)
    | [< ' Token.Number n >] -> Ast.NumLit n
    (* string literal expr *)
    | [< ' Token.String str >] -> Ast.StrLit str
    (* var expr *)
    | [< ' Token.Ident id >] -> Ast.Var id
    (* parentheses as a guiding tool for parsing *)
    | [< ' Token.Kwd '('; e=parse_expr; ' Token.Kwd ')' ?? "expected ')'" >] -> e
    (* let expression *)
    | [< ' Token.LET;
         ' Token.Ident id;
         ' Token.BE ?? "expected 'be' in let";
         e1=parse_expr;
         ' Token.IN_ ?? "expected 'in' in let";
         e2=parse_expr >] -> 
             Ast.Let (e1, id, e2)
    (* strlen expr *)
    | [< ' Token.Kwd '|'; e=parse_expr; ' Token.Kwd '|' ?? "expected closing '|'" >] ->
            Ast.Len e

and parse_bin_rhs expr_prec lhs stream =
    (* Peek the next char in stream and if it is not a single char kwd, just ignore everything beyond it *)
    match Stream.peek stream with
    | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
            let token_prec = precedence c in
            if token_prec < expr_prec then lhs
            else begin
                (* consume token *)
                Stream.junk stream;
                (* get the rhs *)
                let rhs = parse_primary stream in
                (* ... and check operator precedence *)
                let rhs =
                    match Stream.peek stream with
                    | Some (Token.Kwd c2) ->
                            let next_prec = precedence c2 in
                            if token_prec < next_prec
                            then parse_bin_rhs (token_prec + 1) rhs stream
                            else rhs
                    | _ -> rhs
                in
                let lhs =
                    match c with
                    | '+' -> Ast.Plus (lhs, rhs)
                    | '*' -> Ast.Times (lhs, rhs)
                    | '^' -> Ast.Cat (lhs, rhs)
                    | _ -> raise (Stream.Error "unknown binary operator")
                in parse_bin_rhs expr_prec lhs stream
            end
    | _ -> lhs

and parse_expr = parser
    (* parsing the binops *)
    | [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream
    (* catch all error cond *)
    | [< >] -> raise (Stream.Error "unknown token when expecting an expression.")
