open OUnit2;;
open Expression;;


let test1 test_ctxt =
    let example =
        Let ((Cat ((StrLit "bar"), (StrLit "baz"))), "foo", Plus (Plus (NumLit 5, NumLit 6), Times (NumLit 7, Len (Var "foo"))))
    in
    let reducedExample =
        reduceLetExpressions example (Hashtbl.create 100)
    in
    assert_bool "Valid type" (not (checkType example));
    assert_bool "Invalid type" (checkType reducedExample)
;;


let suite =
    "suite">:::
        ["test1">:: test1]
;;


let () =
    run_test_tt_main suite
;;
