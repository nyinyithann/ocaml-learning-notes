open Core
open Common
open UI_display

let open_url_axu link =
  if not (is_whitespace link)
  then (
    let link =
      if String.(is_prefix ~prefix:"http://" link || is_prefix ~prefix:"https://" link)
      then link
      else "https://" ^ link
    in
    match FMConfig.get_open_with () with
    | Ok open_with ->
      (match Browser.get_browser_name open_with with
       | Ok bn -> Caml_unix.execvp "open" [| "open"; "-a"; bn; link |]
       | Error e -> print_error_msg e)
    | Error e -> print_error_msg e)
;;

let open_url ~ch ~data =
  match
    Queue.find data ~f:(fun { Model.mnemonic; _ } ->
      String.(equal (lowercase mnemonic) (lowercase (Char.to_string ch))))
  with
  | Some r ->
    Out_channel.flush stdout;
    (match Caml_unix.fork () with
     | 0 -> open_url_axu r.url
     | _ -> ())
  | _ -> ()
;;
