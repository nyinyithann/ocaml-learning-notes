open Core
module T = ANSITerminal

let ask_input msg =
  T.print_string [ T.Foreground T.Blue ] (sprintf " ✏️  %s" msg);
  printf "%!"
;;

let ask_retry msg = T.print_string [ T.Foreground T.Magenta ] (sprintf " 💪  %s\n" msg)
let print_ok_msg msg = T.print_string [ T.Foreground T.Green ] (sprintf "\n ✅  %s\n" msg)

let print_error_msg msg = T.print_string [ T.Foreground T.Red ] (sprintf "\n 🌶 %s\n" msg)

let rec loop_input ~msg ~retry_msg =
  ask_input msg;
  match In_channel.input_line In_channel.stdin with
  | None | Some "" ->
    ask_retry retry_msg;
    loop_input ~msg ~retry_msg
  | Some x -> x
;;

let prompt_for_url url =
  match url with
  | Some u -> u
  | None -> loop_input ~msg:"Enter a url: " ~retry_msg:"A URL must be provided."
;;

let prompt_for_description desc =
  match desc with
  | Some d -> d
  | None ->
    loop_input ~msg:"Enter description: " ~retry_msg:"Description must be provided."
;;

let prompt_for_category cat =
  match cat with
  | Some c -> c
  | None ->
    loop_input ~msg:"Enter main category: " ~retry_msg:"Main Category must be provided."
;;

let prompt_for_tags tags =
  match tags with
  | Some t -> t
  | None ->
    loop_input
      ~msg:"Enter space-delimited tags : "
      ~retry_msg:"One or more tags must be provided."
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
     let url = prompt_for_url url
     and description = prompt_for_description description
     and category = prompt_for_category category
     and tags = prompt_for_tags tags in
     fun () -> save_bookmark ~url ~description ~category ~tags)
;;
