open Core
open Common
open UI_display
open UI_prompt
module T = ANSITerminal

let get_config_values () =
  let ( let* ) = Result.( >>= ) in
  let* db_path = Config_store.get_db_path () in
  let* browser = Config_store.get_open_with () in
  let* total_count = Data_store.get_total_count () in
  let page_size = Config_store.get_page_size () in
  let config_path = Lazy.force @@ Config_store.get_config_path_full () in
  Ok (config_path, db_path, browser, total_count, page_size)
;;

let get_title title =
  sprintf
    " %s\n %s\n"
    (T.sprintf [ T.Foreground T.Green ] "%s" title)
    (T.sprintf [ T.Foreground T.Green ] "%s" (String.make (String.length title) '='))
;;

let get_sub_info title value =
  sprintf
    "🟢 %s » %s\n%!"
    (T.sprintf [ T.Foreground T.Green ] "%s" title)
    (T.sprintf [ T.Foreground T.Cyan ] "%s" value)
;;

let get_config_info (config_path, db_path, browser, total_count, page_size) =
  sprintf
    "\n%s %s %s %s %s %s"
    (get_title "Configuration Info")
    (get_sub_info "Config file path" config_path)
    (get_sub_info "Db file path" db_path)
    (get_sub_info "Browser to open links" browser)
    (get_sub_info "Total bookmarks in database" @@ string_of_int total_count)
    (get_sub_info "Page size" @@ string_of_int page_size)
;;

let get_tags_info ls =
  sprintf
    "\n%s %s"
    (get_title "Tags")
    (T.sprintf [ T.Foreground T.Cyan ] "%s\n" (String.concat ~sep:" " ls))
;;

let yes_or_no input =
  let si = strip_and_lowercase input in
  String.(si = "y" || si = "n")
;;

let will_show_configs () =
  let msg = "Do you want to see config values (y/n)? " in
  let retry_msg = "Please enter \'y\' or \'n\': " in
  strip_and_lowercase @@ ask_again_if_invalid ~validate:yes_or_no ~msg ~retry_msg ()
;;

let will_show_tags () =
  let msg = "Do you want to see tags (y/n)? " in
  let retry_msg = "Please enter \'y\' or \'n\': " in
  strip_and_lowercase @@ ask_again_if_invalid ~validate:yes_or_no ~msg ~retry_msg ()
;;

let ask_and_display () =
  new_line ();
  let c = will_show_configs () in
  let t = will_show_tags () in
  let r =
    sprintf
      "%s%s"
      (if String.(c = "y")
      then (
        match get_config_values () with
        | Ok s -> sprintf "%s" @@ get_config_info s
        | Error e -> sprintf "%s" e)
      else "")
      (if String.(t = "y")
      then (
        match Data_store.get_tags () with
        | Ok tl -> sprintf "%s" @@ get_tags_info tl
        | Error e -> sprintf "%s" e)
      else "")
  in
  if String.(r = "") then None else Some r
;;

let display ?config_info ?tags_info () =
  if Option.value config_info ~default:false
  then (
    match get_config_values () with
    | Ok c -> printf "%s" @@ get_config_info c
    | Error e -> print_error_msg e);
  if Option.value tags_info ~default:false
  then (
    match Data_store.get_tags () with
    | Ok tl -> printf "%s" @@ get_tags_info tl
    | Error e -> print_error_msg e);
  new_line ()
;;
