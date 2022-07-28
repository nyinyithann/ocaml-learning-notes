open Core
open Core_compat

let add_command =
  Command.basic
    ~summary:"Add a bookmark."
    (let%map_open.Command url =
       flag ~full_flag_required:() "-url" (optional string) ~doc:"string URL to save"
     and tags =
       flag
         ~full_flag_required:()
         "-tags"
         (optional string)
         ~doc:"string Tags for the saving url"
     in
     fun () -> Add_bookmark.add ~url ~tags)
;;

let search_command =
  Command.basic
    ~summary:"Search bookmarks."
    (let%map_open.Command sort_order =
       flag
         ~full_flag_required:()
         "-sort-order"
         (optional string)
         ~doc:"string Sort order (asc or desc)"
     and sort_field =
       flag
         ~full_flag_required:()
         "-sort-field"
         (optional string)
         ~doc:"string One of the fields (url, tags, id, date) to sort against"
     and search_term =
       flag
         ~full_flag_required:()
         "-search-term"
         (optional string)
         ~doc:"string Search term"
     and search_field =
       flag
         ~full_flag_required:()
         "-search-field"
         (optional string)
         ~doc:"string One of the fields (url, tags) to search against"
     in
     fun () ->
       Search_bookmark.search ~search_term ~search_field ~sort_field ~sort_order ())
;;

let ls_command =
  Command.basic
    ~summary:"List bookmarks."
    (let%map_open.Command sort_order =
       flag
         ~full_flag_required:()
         "-sort-order"
         (optional string)
         ~doc:"string Sort order (asc or desc)"
     and sort_field =
       flag
         ~full_flag_required:()
         "-sort-field"
         (optional string)
         ~doc:"string One of the fields (url, tags, id, date) to sort against"
     in
     fun () -> List_bookmark.ls ?sort_field ?sort_order ())
;;

let set_config_command =
  Command.basic
    ~summary:"Set config."
    (let%map_open.Command page_size =
       flag
         ~full_flag_required:()
         "-page-size"
         (optional string)
         ~doc:"number Page size between 1 and 20 (inclusive)."
     and open_with =
       flag
         ~full_flag_required:()
         "-open-with"
         (optional string)
         ~doc:"string Browser name to open bookmarks."
     in
     fun () -> Set_config.set ~page_size ~open_with)
;;

let cmd_group =
  Command.group
    ~summary:"Your favourite bookmarks at your fingertips."
    [ "add", add_command
    ; "search", search_command
    ; "ls", ls_command
    ; "config", set_config_command
    ]
;;

let () = Command_unix.run ~version:"0.0.1" ~build_info:"favemarks ver 0.0.1" cmd_group
