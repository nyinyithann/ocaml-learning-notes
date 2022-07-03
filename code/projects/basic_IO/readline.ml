(* 
    Compile & Run
    -------------

    $ ocamlopt -o readline readline.ml
    $ ./readlin.ml filename_here

 *)
let usage_msg = "readline [-from]" in
let from = ref "" in
let set_filename name = from := name in
let speclist = [("-into", Arg.Set_string from, "Set file name to read from")] in
let () = 
    Arg.parse speclist set_filename usage_msg in
    let inc = open_in !from in
    try 
        let line = input_line inc in
        print_endline line ;
        flush stdout ;
        close_in inc ;
    with _ ->
        close_in_noerr inc ;
        prerr_endline "Error occured"

