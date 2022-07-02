let usage_msg = "writeline [-into]" in
let into = ref "" in
let set_filename name = into := name in
let speclist = [("-into", Arg.Set_string into, "Set output file name")] in
let () = 
    Arg.parse speclist set_filename usage_msg in 
    let filename = Printf.sprintf "%s.txt" !into in
    Printf.printf "Enter a line to save into %s.txt file.\n" filename ;
    try
        let line = read_line() in
        let out = open_out filename in
        Printf.fprintf out "%s\n" line ;
        close_out out;
    with _ ->
        prerr_endline "Error occured."


(*
    Compile & Run
    -------------

    $ ocamlopt -o writeline writeline.ml
    $ ./writeline -into filename_here
*)
