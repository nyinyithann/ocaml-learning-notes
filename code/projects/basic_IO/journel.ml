(*
    Serialize records using Mashal module and save them to a file.
    Read them back from the file, unmarshal and display them.
    
    Compile & Run
    -------------

    $ ocamlopt -o journel unix.cmxa jtypes.ml journel.ml
    $ ./journel -into filename_here
*)
let usage_msg = "journel [-into]" in
let filename = ref "" in
let set_filename name = filename := name in
let speclist = [("-into", Arg.Set_string filename, "Set output file name")] in
let write () = 
    let file = open_out_gen [Open_creat; Open_append; Open_binary; Open_nonblock] 740 !filename in
    try
        print_endline "Enter title: " ;
        let title = read_line () in
        print_endline "Enter journel: " ;
        let note = read_line ()  in
        let j = {
            Jtypes.title = title ;
            note ;
            time = Unix.localtime (Unix.time())
        }  in
        let data = Marshal.to_string j [] in
        Printf.fprintf file "%s\n" data ;
        flush file ;
        close_out file;
    with e ->
        close_out_noerr file ;
        raise e  
    and read () =
        Printf.printf "Reading from the file %s.\n" !filename;
        let file = open_in_gen [Open_rdonly; Open_binary; Open_nonblock] 644 !filename in
        try
            let try_read () = try Some(input_line file) with End_of_file -> None in
            let rec loop acc =
                match try_read() with
                | Some s -> loop (s :: acc)
                | None -> close_in file; List.rev acc in
            let lines : string list = loop [] in
            Printf.printf "Total records = %d.\n" (List.length lines) ;
            lines |> List.iter (fun s -> 
                let j : Jtypes.journel = Marshal.from_string s 0 in
                Printf.printf "\t{title = %s; note = %s; time = %s}\n" j.title j.note 
                (Printf.sprintf ("%d/%d/%d") j.time.tm_wday j.time.tm_mon j.time.tm_year);
            ) ;
        with e ->
            close_in_noerr file;
            raise e
    in

let () = 
    Arg.parse speclist set_filename usage_msg in 
    try
        write () ;
        read ();
    with _ ->
        prerr_endline "Error occured!"
