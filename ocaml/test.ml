open OUnit2;;

let test1 test_ctxt =
    let s = new Istack.istack in
    assert_equal None (s#peek)
;;

let suite =
    "suite">:::
        ["test1">:: test1]
;;

let () =
    run_test_tt_main suite
;;
