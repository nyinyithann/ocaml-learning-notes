open Core
open Core_compat

let add_command =
  Command.basic
    ~summary:"add a bookmark."
    (let%map_open.Command url =
       flag ~full_flag_required:() "-url" (optional string) ~doc:"string url to save"
     and tags =
       flag
         ~full_flag_required:()
         "-tags"
         (optional string)
         ~doc:"string tags for the saving url"
     in
     fun () -> Add_bookmark.add ~url ~tags)
;;

let search_command =
  Command.basic
    ~summary:"search bookmarks."
    (let%map_open.Command sort_order =
       flag
         ~full_flag_required:()
         "-sort-order"
         (optional string)
         ~doc:"string sort order (asc or desc)"
     and sort_field =
       flag
         ~full_flag_required:()
         "-sort-field"
         (optional string)
         ~doc:"string one of the fields (url, tags, id, date) to sort against"
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
         ~doc:"string one of the fields (url, tags) to search against"
     in
     fun () ->
       ListSearch_bookmarks.Search.search
         ~search_term
         ~search_field
         ~sort_field
         ~sort_order
         ())
;;

let ls_command =
  Command.basic
    ~summary:"list bookmarks."
    (let%map_open.Command sort_order =
       flag
         ~full_flag_required:()
         "-sort-order"
         (optional string)
         ~doc:"string sort order (asc or desc)"
     and sort_field =
       flag
         ~full_flag_required:()
         "-sort-field"
         (optional string)
         ~doc:"string one of the fields (url, tags, id, date) to sort against"
     in
     fun () ->
       let sf = Option.value sort_field ~default:"id" in
       let so = Option.value sort_order ~default:"desc" in
       ListSearch_bookmarks.Ls.ls ~sort_field:sf ~sort_order:so ())
;;

let set_config_command =
  Command.basic
    ~summary:"set config."
    (let%map_open.Command page_size =
       flag
         ~full_flag_required:()
         "-page-size"
         (optional string)
         ~doc:"number page size between 1 and 20 (inclusive)."
     and open_with =
       flag
         ~full_flag_required:()
         "-open-with"
         (optional string)
         ~doc:"string browser name to open bookmarks."
     in
     fun () -> Set_config.set ~page_size ~open_with)
;;

let info_command =
  Command.basic
    ~summary:"show info."
    (let%map_open.Command config_info =
       flag
         ~full_flag_required:()
         "-config-info"
         (optional bool)
         ~doc:"bool configuration info."
     and tags_info =
       flag
         ~full_flag_required:()
         "-tags-info"
         (optional bool)
         ~doc:"bool all the tags stored in database."
     in
     fun () -> Display_info.display ?config_info ?tags_info ())
;;

let cmd_group =
  Command.group
    ~summary:"Your favourite bookmarks at your fingertips."
    [ "add", add_command
    ; "search", search_command
    ; "ls", ls_command
    ; "config", set_config_command
    ; "info", info_command
    ]
;;

let () = Command_unix.run ~version:"0.0.1" ~build_info:"favemarks ver 0.0.1" cmd_group
