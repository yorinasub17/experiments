class istack = object
    val mutable v : int list = []

    method pop =
        match v with
        | hd :: tl ->
                v <- tl;
                Some hd
        | [] -> None

    method push hd =
        v <- hd :: v

    method peek =
        match v with
        | hd :: tl ->
                Some hd
        | [] -> None
    
    method view =
        v

    method isEmpty =
        match v with
        | [] -> true
        | _ -> false

end ;;
