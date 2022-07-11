open Core
open Core_compat

let do_hash hash_length file =
    Md5.digest_file_blocking file 
        |> Md5.to_hex  
        |> (fun s -> String.prefix s hash_length)
        |> print_endline 

let command =
    Command.basic
        ~summary:"Generate an MD5 hash of the input data"
        ~readme:(fun () -> "More detailed information")
        (let%map_open.Command hash_length = anon ("hash_length" %: int)
        and filename = anon ("filename" %: Filename_unix.arg_type) in
        fun () -> do_hash hash_length filename)

let () = 
    Command_unix.run ~version:"0.0.1" ~build_info:"RWO" command |> ignore 
