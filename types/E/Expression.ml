type etype =
    | NUM
    | STR
    | NONE

type expression =
    (* The string is the name of the variable *)
    | Var of string
    (* The int is the value of the literal *)
    | NumLit of int
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

type typedExpression = expression * etype


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

(* TODO: all of the below is hard coded type rules... How do I generalize? *)
let rec assignType e = match e with
      NumLit _ -> (e, NUM)
    | StrLit _ -> (e, STR)
    | Plus (_, _) -> (e, NUM)
    | Times (_, _) -> (e, NUM)
    | Cat (_, _) -> (e, STR)
    | Len _ -> (e, NUM)
    (* Let is not here, because we should have reduced the let expression before assigning the types *)
    | _ -> (e, NONE)

let checkMath e1 e2 =
    let (_, t1) = assignType e1 in
    let (_, t2) = assignType e2 in
    if t1 == NUM && t2 == NUM then
        true
    else
        false

let checkCat e1 e2 =
    let (_, t1) = assignType e1 in
    let (_, t2) = assignType e2 in
    if t1 == STR && t2 == STR then
        true
    else
        false

let checkLen e =
    let (_, t1) = assignType e in
    t1 == STR

let rec checkType e = match e with
      NumLit _ -> true
    | StrLit _ -> true 
    | Plus (e1, e2) -> checkType e1 && checkType e2 && checkMath e1 e2
    | Times (e1, e2) -> checkType e1 && checkType e2 && checkMath e1 e2
    | Cat (e1, e2) -> checkType e1 && checkType e2 && checkCat e1 e2
    | Len e -> checkType e && checkLen e
    (* All other expressions are false *)
    | _ -> false
