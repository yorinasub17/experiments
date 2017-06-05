let rec main_loop stream =
    match Stream.peek stream with
    | None -> ()
    | Some (Token.Kwd ';') ->
            Stream.junk stream;
            main_loop stream
    | Some token ->
            begin 
                try
                    let e = Parser.parse_expr stream in
                    let e_str = Ast.string_of_expr e in
                    let reduced_e = Ast.reduce e in
                    let reduced_e_str = Ast.string_of_expr reduced_e in
                    let typed_e = Type.assignType reduced_e in
                    let typed_e_str = Type.string_of_typed_expr typed_e in
                    let valid = Type.checkType typed_e in
                    print_endline "";
                    print_endline "parsed expression!";
                    print_endline e_str;
                    print_endline "reduced to:";
                    print_endline reduced_e_str;
                    print_endline "type checked to:";
                    print_endline typed_e_str;
                    print_endline ("validity: " ^ string_of_bool valid);
                    print_endline ""
                with Stream.Error s ->
                    Stream.junk stream;
                    print_endline s;
            end;
            print_string "ready> "; flush stdout;
            main_loop stream
