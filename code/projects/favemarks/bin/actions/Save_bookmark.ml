open Core
open UI_display
open UI_prompt
open Common

let get_url v =
  let msg = "Enter a url: "
  and retry_msg = "A valid url must be provided." in
  match v with
  | None -> ask_again_if_invalid ~validate:validate_url ~msg ~retry_msg ()
  | Some x ->
    if validate_url x
    then String.strip x
    else ask_again_if_invalid ~validate:validate_url ~retry_first:() ~msg ~retry_msg ()
;;

let get_tags v =
  let msg = "Enter comma-delimited tags: "
  and retry_msg =
    "One or more comma-delimited tags must be provided. Tags should not have space."
  in
  (match v with
   | None -> ask_again_if_invalid ~validate:validate_tags ~msg ~retry_msg ()
   | Some x ->
     if validate_tags x
     then String.strip x
     else ask_again_if_invalid ~validate:validate_tags ~retry_first:() ~msg ~retry_msg ())
  |> strip_space_and_concat ~sep:","
;;

let save ~url ~tags =
  let url = get_url url
  and tags = get_tags tags in
  match Db.save ~url ~tags with
  | Result.Ok s -> print_ok_msg s
  | Result.Error e -> print_error_msg e
;;
