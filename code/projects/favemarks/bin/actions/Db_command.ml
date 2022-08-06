open Core
open UI_display

let db_new ~path =
  match Data_store.db_new ~path with
  | Ok s -> print_ok_msg s
  | Error e -> print_error_msg e
;;

let db_switch ~new_path =
  match Config_store.set_db_filepath ~new_path with
  | Ok s -> print_ok_msg s
  | Error e -> print_error_msg e
;;
