open Core
open Common

let get_url v =
  let msg = "Enter a url: "
  and retry_msg = "A valid url must be provided." in
  match v with
  | None -> ask_again_if_invalid ~validate:validate_url ~msg ~retry_msg ()
  | Some x ->
    if validate_url x
    then x
    else ask_again_if_invalid ~validate:validate_url ~retry_first:() ~msg ~retry_msg ()
;;

let get_tags v =
  let msg = "Enter comma-delimited tags : "
  and retry_msg = "One or more tags must be provided."
  and validate input = not (is_whitespace input) in
  (match v with
  | None -> ask_again_if_invalid ~validate ~msg ~retry_msg ()
  | Some x ->
    if validate x
    then x
    else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ())
  |> strip_space_and_concat ~sep:","
;;

let save_bookmark ~url ~tags =
  let url = String.strip url
  and tags = String.strip tags in
  match Db.save ~url ~tags with
  | Result.Ok s -> print_ok_msg s
  | Result.Error e -> print_error_msg e
;;

let command =
  Command.basic
    ~summary:"Save a bookmark."
    (let%map_open.Command url =
       flag ~full_flag_required:() "-url" (optional string) ~doc:"string URL to save"
     and tags =
       flag
         ~full_flag_required:()
         "-tags"
         (optional string)
         ~doc:"string Tags for the saving url"
     in
     let url = get_url url
     and tags = get_tags tags in
     fun () -> save_bookmark ~url ~tags)
;;
