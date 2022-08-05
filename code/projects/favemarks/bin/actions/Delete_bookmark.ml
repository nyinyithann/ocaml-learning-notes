open Core
open Common
open UI_display
open UI_prompt

let get_key ~msg bookmarks =
  let retry_msg = "Key is not found in the displaying records. Please try again." in
  let keys = bookmarks |> List.map ~f:(fun x -> x.Model.mnemonic) in
  let validate input = validate_fields keys input in
  strip_and_lowercase @@ ask_again_or_default ~validate ~msg ~retry_msg ""
;;

let confirm () =
  let msg = "Are you sure to delete (yes or n)? " in
  let retry_msg = "Please entry \'yes\' or \'n\': " in
  let validate input =
    let si = strip_and_lowercase input in
    String.(si = "yes" || si = "n")
  in
  match ask_again_if_invalid ~validate ~msg ~retry_msg () with
  | "yes" -> true
  | _ -> false
;;

let delete ~go_home ~state =
  new_line ();
  let bookmarks = State.get_bookmarks state in
  let input = get_key ~msg:"Enter key or nothing to skip: " bookmarks in
  if String.(input = "")
  then (
    State.set_status state None;
    go_home ~state)
  else (
    let r =
      List.find bookmarks ~f:(fun x -> String.(x.Model.mnemonic = input))
      |> Option.value_exn
    in
    if confirm ()
    then (
      match Data_store.delete ~id:r.Model.id with
      | Ok s -> State.set_status state @@ Some (sprintf "%s, %s." s r.url)
      | Error e -> State.set_status state @@ Some e)
    else State.set_status state None;

    go_home ~state)
;;
