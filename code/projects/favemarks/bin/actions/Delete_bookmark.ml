open Core
open UI_display

let delete data =
  new_line ();
  let id = Update_bookmark.get_id ~msg:"Enter id to delete: " data in
  match Db.delete ~id:(Int.of_string id) with
  | Result.Ok s -> print_ok_msg s
  | Result.Error e -> print_error_msg e
;;
