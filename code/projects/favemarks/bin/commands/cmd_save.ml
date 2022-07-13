open Core
open Common

let validate input = not (is_whitespace input)

let get_value ~v ~msg ~retry_msg =
  match v with
  | None -> ask_again_if_invalid ~validate ~msg ~retry_msg ()
  | Some x ->
    if validate x
    then x
    else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()
;;

let get_url v =
  let msg = "Enter a url: "
  and retry_msg = "A URL must be provided." in
  get_value ~v ~msg ~retry_msg
;;

let get_description v =
  let msg = "Enter description: "
  and retry_msg = "Description must be provided." in
  get_value ~v ~msg ~retry_msg
;;

let get_category v =
  let msg = "Enter main category: "
  and retry_msg = "Main Category must be provided." in
  get_value ~v ~msg ~retry_msg
;;

let get_tags v =
  let msg = "Enter space-delimited tags : "
  and retry_msg = "One or more tags must be provided." in
  get_value ~v ~msg ~retry_msg
;;

let save_bookmark ~url ~description ~category ~tags =
  let url = String.strip url
  and description = String.strip description
  and category = String.strip category
  and tags = String.strip tags in
  match Db.save ~url ~description ~category ~tags with
  | Result.Ok s -> print_ok_msg s
  | Result.Error e -> print_error_msg e
;;

let command =
  Command.basic
    ~summary:"Save a bookmark."
    (let%map_open.Command url =
       flag ~full_flag_required:() "-url" (optional string) ~doc:"string URL to save"
     and description =
       flag
         ~full_flag_required:()
         "-des"
         (optional string)
         ~doc:"string Description for the saving url"
     and category =
       flag
         ~full_flag_required:()
         "-cat"
         (optional string)
         ~doc:"string Main Category for the saving url"
     and tags =
       flag
         ~full_flag_required:()
         "-tags"
         (optional string)
         ~doc:"string Tags for the saving url"
     in
     let url = get_url url
     and description = get_description description
     and category = get_category category
     and tags = get_tags tags in
     fun () -> save_bookmark ~url ~description ~category ~tags)
;;
