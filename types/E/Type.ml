type etype =
    | NUM
    | STR
    | NONE

type typedExpressionBase =
    | TypedVar of string
    | TypedNumLit of float
    | TypedStrLit of string
    | TypedPlus of (etype * typedExpressionBase) * (etype * typedExpressionBase)
    | TypedTimes of (etype * typedExpressionBase) * (etype * typedExpressionBase)
    | TypedCat of (etype * typedExpressionBase) * (etype * typedExpressionBase)
    | TypedLen of (etype * typedExpressionBase)
    | TypedLet of (etype * typedExpressionBase) * string * (etype * typedExpressionBase)

type typedExpression = etype * typedExpressionBase


(* TODO: all of the below is hard coded type rules... How do I generalize? *)
let rec assignType e = match e with
      Ast.NumLit k -> (NUM, TypedNumLit k)
    | Ast.StrLit k -> (STR, TypedStrLit k)
    | Ast.Plus (e1, e2) ->
            (NUM, TypedPlus ((assignType e1), (assignType e2)))
    | Ast.Times (e1, e2) ->
            (NUM, TypedTimes ((assignType e1), (assignType e2)))
    | Ast.Cat (e1, e2) ->
            (STR, TypedCat ((assignType e1), (assignType e2)))
    | Ast.Len k ->
            (NUM, TypedLen (assignType k))
    (* Var is not valid, because there should be no variables left after let substitution *)
    (* Let is not valid, because they should all be reduced *)
    | Ast.Var varname -> (NONE, TypedVar varname)
    | Ast.Let (e1, varname, e2) -> (NONE, TypedLet (assignType e1, varname, assignType e2))

let rec checkType typedE = match typedE with
      (NUM, TypedNumLit _) -> true
    | (STR, TypedStrLit _) -> true
    | (NUM, TypedPlus ((NUM, e1), (NUM, e2))) ->
            (checkType (NUM, e1)) && (checkType (NUM, e2))
    | (NUM, TypedTimes ((NUM, e1), (NUM, e2))) ->
            (checkType (NUM, e1)) && (checkType (NUM, e2))
    | (STR, TypedCat ((STR, e1), (STR, e2))) ->
            (checkType (STR, e1)) && (checkType (STR, e2))
    | (NUM, TypedLen (STR, e)) ->
            checkType (STR, e)
    (* if we match anything else, then it is a type error *)
    | _ -> false


let rec string_of_type typ =
    match typ with
    | NUM -> "NUM"
    | STR -> "STR"
    | NONE -> "NONE"

let rec string_of_typed_expr (t, typed_expr) =
    match typed_expr with
    | TypedVar id -> 
            id ^ " : " ^ string_of_type t
    | TypedNumLit flt ->
            "num[" ^ string_of_float flt ^ "] : " ^ string_of_type t
    | TypedStrLit str ->
            "str[" ^ str ^ "] : " ^ string_of_type t
    | TypedPlus ((t1, typedE1), (t2, typedE2)) ->
            "plus(" ^ string_of_typed_expr (t1, typedE1) ^ "; " ^ string_of_typed_expr (t2, typedE2) ^ ") : " ^ string_of_type t
    | TypedTimes ((t1, typedE1), (t2, typedE2)) ->
            "times(" ^ string_of_typed_expr (t1, typedE1) ^ "; " ^ string_of_typed_expr (t2, typedE2) ^ ") : " ^ string_of_type t
    | TypedCat ((t1, typedE1), (t2, typedE2)) ->
            "cat(" ^ string_of_typed_expr (t1, typedE1) ^ "; " ^ string_of_typed_expr (t2, typedE2) ^ ") : " ^ string_of_type t
    | TypedLen (t1, typedE1) ->
            "len(" ^ string_of_typed_expr (t1, typedE1) ^ ") : " ^ string_of_type t
    | TypedLet ((t1, typedE1), varname, (t2, typedE2)) ->
            "let(" ^ string_of_typed_expr (t1, typedE1) ^ "; " ^ varname ^ ".(" ^ string_of_typed_expr (t2, typedE2) ^ ")) : " ^ string_of_type t
