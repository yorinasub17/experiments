open OUnit2;;
open Untyped;;
open Typed;;


let test1 test_ctxt =
    let example =
        Let ((Cat ((StrLit "bar"), (StrLit "baz"))), "foo", Plus (Plus (NumLit 5, NumLit 6), Times (NumLit 7, Len (Var "foo"))))
    in
    let typedExample =
        assignType example
    in
    let reducedExample =
        reduceLetExpressions example (Hashtbl.create 100)
    in
    let typedReducedExample =
        assignType reducedExample
    in
    assert_bool "Valid type" (not (checkType typedExample));
    assert_bool "Invalid type" (checkType typedReducedExample)
;;


let test2 test_ctxt =
    let example =
        Let ((Cat ((StrLit "bar"), (StrLit "baz"))), "foo", Plus (Plus (NumLit 5, NumLit 6), Times (NumLit 7, Len (Var "foo"))))
    in
    let expectedReducedExample =
        Plus (Plus (NumLit 5, NumLit 6), Times (NumLit 7, Len (Cat ((StrLit "bar"), (StrLit "baz")))))
    in
    assert_equal (reduceLetExpressions example (Hashtbl.create 100)) expectedReducedExample
;;


let test3 test_ctxt =
    let example =
        Plus (Plus (NumLit 5, NumLit 6), Times (NumLit 7, Len (Cat ((StrLit "bar"), (StrLit "baz")))))
    in
    let expectedTypedExample =
        (NUM,
         TypedPlus
         ((NUM, TypedPlus ((NUM, TypedNumLit 5), (NUM, TypedNumLit 6))),
          (NUM, TypedTimes ((NUM, TypedNumLit 7),
                            (NUM, TypedLen (STR,
                                            TypedCat ((STR, TypedStrLit "bar"),
                                                      (STR, TypedStrLit "baz"))))))))
    in
    assert_equal (assignType example) expectedTypedExample
;;


let suite =
    "suite">:::
        ["test1">:: test1;
         "test2">:: test2;
         "test3">:: test3]
;;


let () =
    run_test_tt_main suite
;;
