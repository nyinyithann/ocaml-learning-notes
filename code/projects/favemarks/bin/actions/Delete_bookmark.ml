open Core
open Common
open UI_display
open UI_prompt

let get_key ~msg data =
  let retry_msg = "Key is not found in the displaying records. Please try again." in
  let keys = Queue.to_list data |> List.map ~f:(fun x -> x.Model.mnemonic) in
  let validate input = validate_fields keys input in
  strip_and_lowercase @@ ask_again_or_default ~validate ~msg ~retry_msg ""
;;

let delete ~go_home ~data =
  new_line ();
  let input = get_key ~msg:"Enter key or nothing to skip: " data in
  if String.(input = "")
  then go_home ()
  else (
    let r =
      Queue.find data ~f:(fun x -> String.(x.Model.mnemonic = input)) |> Option.value_exn
    in
    (match Db.delete ~id:r.Model.id with
     | Result.Ok s -> print_ok_msg s
     | Result.Error e -> print_error_msg e);
    go_home ())
;;
