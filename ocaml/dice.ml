open Core.Std

let _ = Random.self_init ()

let roll _ = Random.int 6

let () =
    printf "Enter quit to quit\n%!"
    while
        let line = In_channel.input_line In_channel.stdin in
        match line with
        | None -> false
        | Some x -> String.strip x <> "quit"
    do
        printf "Foozle: %d, %d\n%!" (roll ()) (roll ())
    done
