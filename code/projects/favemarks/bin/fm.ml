open Core_compat
open Core
open Sqlite3

let db_uri =
  "/Users/jazz/ghq/github.com/nyinyithann/notes_on_ocaml/code/projects/favemarks/db/favemarks.db"
;;

let save_bookmark ~url ~description ~main_category ~tags =
  match url, description, main_category, tags with
  | Some u, Some d, Some c, Some t ->
    let t' =
      t |> String.split ~on:',' |> List.map ~f:String.strip |> String.concat ~sep:","
    in
    let db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let close_db () = db_close db |> ignore in
    (try
       let sql =
         sprintf
           "INSERT INTO bookmarks(link, description, category, tags, added) VALUES('%s', \
            '%s', '%s', '%s', '%s')"
           u
           d
           c
           t'
           (Time.now () |> Time.to_string_utc)
       in
       (match exec db sql with
       | Rc.OK -> printf "Row inserted with id %Ld\n" (last_insert_rowid db)
       | e ->
         prerr_endline (Rc.to_string e);
         prerr_endline (errmsg db));
       close_db ()
     with
    | _ -> close_db ())
  | _ -> raise (Invalid_argument "save_bookmark")
;;

let rec loop_input ~msg ~retry_msg =
  printf "%s %!" msg;
  match In_channel.input_line In_channel.stdin with
  | None | Some "" ->
    printf "%s\n" retry_msg;
    loop_input ~msg ~retry_msg
  | Some _ as x -> x
;;

let prompt_for_url url =
  match url with
  | Some _ as u -> u
  | None -> loop_input ~msg:"Enter a url: " ~retry_msg:"A URL must be provided."
;;

let prompt_for_description desc =
  match desc with
  | Some _ as d -> d
  | None ->
    loop_input ~msg:"Enter description: " ~retry_msg:"Description must be provided."
;;

let prompt_for_main_category cat =
  match cat with
  | Some _ as c -> c
  | None ->
    loop_input ~msg:"Enter main category: " ~retry_msg:"Main Category must be provided."
;;

let prompt_for_tags tags =
  match tags with
  | Some _ as t -> t
  | None -> loop_input ~msg:"Enter tags: " ~retry_msg:"One or more tags must be provided."
;;

let save_bookmark_command =
  Command.basic
    ~summary:"Save a bookmark."
    (let%map_open.Command url = flag "-u" (optional string) ~doc:"string URL to save"
     and description =
       flag "-d" (optional string) ~doc:"string Description for the saving url"
     and main_category =
       flag "-c" (optional string) ~doc:"string Main Category for the saving url"
     and tags = flag "-t" (optional string) ~doc:"string Tags for the saving url" in
     let url = prompt_for_url url
     and description = prompt_for_description description
     and main_category = prompt_for_main_category main_category
     and tags = prompt_for_tags tags in
     fun () -> save_bookmark ~url ~description ~main_category ~tags)
;;

let cmd_group =
  Command.group
    ~summary:"🐫 A command-line app to save your favourite bookmarks 🐫"
    [ "save", save_bookmark_command ]
;;

let () = Command_unix.run ~version:"0.0.1" ~build_info:"favemarks" cmd_group
