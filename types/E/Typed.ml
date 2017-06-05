type etype =
    | NUM
    | STR
    | NONE

type typedExpressionBase =
    | TypedVar of string
    | TypedNumLit of int
    | TypedStrLit of string
    | TypedPlus of (etype * typedExpressionBase) * (etype * typedExpressionBase)
    | TypedTimes of (etype * typedExpressionBase) * (etype * typedExpressionBase)
    | TypedCat of (etype * typedExpressionBase) * (etype * typedExpressionBase)
    | TypedLen of (etype * typedExpressionBase)
    | TypedLet of (etype * typedExpressionBase) * string * (etype * typedExpressionBase)

type typedExpression = etype * typedExpressionBase


(* TODO: all of the below is hard coded type rules... How do I generalize? *)
let rec assignType e = match e with
      Untyped.NumLit k -> (NUM, TypedNumLit k)
    | Untyped.StrLit k -> (STR, TypedStrLit k)
    | Untyped.Plus (e1, e2) ->
            (NUM, TypedPlus ((assignType e1), (assignType e2)))
    | Untyped.Times (e1, e2) ->
            (NUM, TypedTimes ((assignType e1), (assignType e2)))
    | Untyped.Cat (e1, e2) ->
            (STR, TypedCat ((assignType e1), (assignType e2)))
    | Untyped.Len k ->
            (NUM, TypedLen (assignType k))
    (* Var is not valid, because there should be no variables left after let substitution *)
    (* Let is not valid, because they should all be reduced *)
    | Untyped.Var varname -> (NONE, TypedVar varname)
    | Untyped.Let (e1, varname, e2) -> (NONE, TypedLet (assignType e1, varname, assignType e2))

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
