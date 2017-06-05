type expression =
    (* The string is the name of the variable *)
    | Var of string
    (* The int is the value of the literal *)
    | NumLit of float
    (* The string is the value of the literal *)
    | StrLit of string
    (* The second expression is added to the first expression
     * and the same with the times for mult *)
    | Plus of expression * expression
    | Times of expression * expression
    | Cat of expression * expression
    (* Evaluates the length of the expression, if string *)
    | Len of expression
    (* First expression is substituted for variable with name (string) in the second expression *)
    | Let of expression * string * expression


(* TODO: Is this really the right way to deal with let expressions? *)
let substituteVar substitutions varname = Hashtbl.find substitutions varname

let rec reduceLetExpressions e substitutions = match e with
      Let (e1, varname, e2) ->
          let reducedE1 = reduceLetExpressions e1 substitutions in
          Hashtbl.add substitutions varname reducedE1;
          reduceLetExpressions e2 substitutions
    | Var (varname) -> substituteVar substitutions varname
    | Plus (e1, e2) ->
          let reducedE1 = reduceLetExpressions e1 substitutions in
          let reducedE2 = reduceLetExpressions e2 substitutions in
          Plus (reducedE1, reducedE2)
    | Times (e1, e2) ->
          let reducedE1 = reduceLetExpressions e1 substitutions in
          let reducedE2 = reduceLetExpressions e2 substitutions in
          Times (reducedE1, reducedE2)
    | Cat (e1, e2) ->
          let reducedE1 = reduceLetExpressions e1 substitutions in
          let reducedE2 = reduceLetExpressions e2 substitutions in
          Cat (reducedE1, reducedE2)
    | Len e1 ->
          let reducedE1 = reduceLetExpressions e1 substitutions in
          Len reducedE1
    | k -> k

let reduce expr =
    reduceLetExpressions expr (Hashtbl.create 100)

let rec string_of_expr expr =
    match expr with
    | Var id -> id
    | NumLit flt -> "num[" ^ string_of_float flt ^ "]"
    | StrLit str -> "str[" ^ str ^ "]"
    | Plus (e1, e2) -> "plus(" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ ")"
    | Times (e1, e2) -> "times(" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ ")"
    | Cat (e1, e2) -> "cat(" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ ")"
    | Len e -> "len(" ^ string_of_expr e ^ ")"
    | Let (e1, id, e2) -> "let(" ^ string_of_expr e1 ^ "; " ^ id ^ ".(" ^ string_of_expr e2 ^ "))"
